local gen_server = require "gen_server"
local UI = require "lib.ui"
local Bundle = require "lib.bundle"
local Graphic = require "lib.graphic"
local Menu = require 'lib.ui_menu'
local Panel = require "lib.ui_obj"
local Airlock = require 'airlock'
local Door = require 'door'
local static_ui = require "static_ui"
local audio_player = require "audio_player"
local elevator = require 'elevator'

local Program = {}

local CABLE_SIDE = "back"
local cables = {
  
}

local function powerDown()
  local x,y,z = 103,71,2
  local telesound = "/playsound fdi:event.part1.level1_powerdown @a %d %d %d 30 1"
  exec(telesound,x,y,z)
end

local function teleSound()
  local x,y,z = 134,71,10
  local telesound = "/playsound fdi:event.part0.teleport_overworld @a[%d,%d,%d,20]"
  exec(telesound,x,y,z)
end

---------------
--Server & UI--
---------------

local function enable(panel,button,index)
  local i = index or 2
  local old = panel.index[i]
  if old == button then return end
  old.reactor:stop()
  button.reactor:start()
  panel:replace(old,button)
end

--resets color of a disabled button
local function reenable(panel,button,index)
  local old = panel.index[index]
  if old ~= button then
    old.reactor:stop()
    panel:replace(old,button)
  end
  button:setTextColor(nil)
  button.reactor:start()
end

local function disable(panel,index)
  local obj = panel.index[index]
  obj:setTextColor(colors.lightGray)
--  obj.reactor:stop()
end

local function initUI(monitor)
  local ui = UI.start(monitor,29,9)
  ui:align("center","left")
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  
  local title = Graphic:new("TELEPORTER CONTROLS")
  
  local open = Graphic:new("Open Chamber")
  local close = Graphic:new("Close Chamber")
  local teleport = Graphic:new("Activate Teleporter")
  title:align("center")
  title.ypos = 2
  local menu = Panel:new()
  
  menu.width = "max"
  
  open.xpos = 6
  close.xpos = 6
  teleport.xpos = 6
--  menu.xpos = 6
--  menu.proto.backgroundFocus = colors.gray
--  menu.proto.textFocus = colors.white
  
  menu:add(close)
  menu:add(teleport)
--  menu:link(ui)
  menu:setBackgroundColor(colors.gray)
  menu:setTextColor(colors.white)
  
  ui:add(title)
  local spacer = Graphic:new("                             ")
  spacer:setBackgroundColor(colors.gray)
  spacer.width = "max"
  ui:add(spacer)
  ui:add(menu)
  ui:update()
  
  ui.openedSilo = function()
    enable(menu,close,1)
    disable(menu,2)
    ui:update()
  end
  
  ui.closingSilo = function()
    enable(menu,open,1)
    ui:update()
  end
  
  ui.closedSilo = function()
    reenable(menu,teleport,2)
    ui:update()
  end

  ui.teleporting = function()
--    disable(menu,1)
--    ui:update()
  end
  
  local openSilo = function()
    if Program.openSilo() then
      ui:ping()
    else
      ui:beep()
    end
  end
  
  local closeSilo = function()
    Program.closeSilo()
    ui:ping()
  end
  
  local runTele = function()
    if Program.teleport() then
      ui:ping()
    else
      ui:beep()
    end
  end
  
  ui.denied = function()
    ui:beep()
  end
  
--  open:setJustOnSelect(ui,openSilo)
--  close:setJustOnSelect(ui,closeSilo)
--  teleport:setJustOnSelect(ui,runTele)
--  
  open:setOnSelect(ui,openSilo)
  close:setOnSelect(ui,closeSilo)
  teleport:setOnSelect(ui,runTele)
  
  disable(menu,2)
  ui:update()
  
  return ui
end


local function callPanelUI(monitor,floor)
  local ui = UI.start(monitor,7,5)
  
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  
  local title = Graphic:new("Level "..floor)
  title.align = "center"
  local status = Graphic:new("       ")
  
  local body = Panel:new()
  body:setLayout("static")
  body.width = "max"
    
  local callButton = Graphic:new("Call")
  callButton.xpos = 3
  callButton.ypos = 2 
  
  status.xpos = 1
  status.ypos = 3
  status:setTextColor(colors.red)
  
  
  body:add(callButton)
  body:add(status)
  body:setHeight(3)
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.orange)
  ui:add(title)
  ui:add(body)
  ui:update()
  
  
  local lastDenied = nil
  local denyTime
  local flashDenied = function()
    EVE.sleep(denyTime or 1)
    denyTime = nil
    if lastDenied == VM.running() then
      status.text="       "
      ui:update()
      lastDenied = nil
    end
  end
  
  local denyAccess = false
  local function deny()
    ui:beep()
    status.text="Denied!"
    ui:update()
    lastDenied = VM.spawn(flashDenied)
  end
  
  body:setOnSelect(ui,deny)
  
  return ui, callButton
end

function Program.init()
  local overheadLights = Bundle:new("back",colors.lightGray)
  overheadLights:enable()
  
  return true, {
    lights = overheadLights,
    reset = Bundle:new("back",colors.magenta)
  }
end

function Program.handle_call(Request,From,State)
  local event = Request[1]
  if event == "open_silo" then
    if State.teleporting then
      gen_server.reply(From,false)
    else
      gen_server.reply(From,true)
      Door.open(State.teleporter)
    end
  elseif event == "teleport" then
    if State.teleporting or State.siloOpen then
      gen_server.reply(From,false)
    else
      gen_server.reply(From,true)
      State.ui.teleporting()
      State.teleporting = true
      VM.spawnlink(function() runTeleporter(State.emitter,State.teleporter) end)
    end    
  end
  return State
end

function Program.handle_cast(Request,State)
  local event = Request[1]
  if event == "close_silo" then
    Door.close(State.teleporter)
  elseif event == "opened" then
    State.ui.openedSilo()
    State.siloOpen = true
  elseif event == "closing" then
    State.ui.closingSilo()
  elseif event == "closed" then
    State.ui.closedSilo()
    State.siloOpen = false
  elseif event == "done_teleport" then
    State.teleporting = false
    Door.open(State.teleporter)
  elseif event == "redstone" then
    if State.detector:isOn() and not State.outage then
      State.outage = true
      VM.spawn(function() runOutage(State.lights) end)
    elseif State.reset:isOn() then
      State.outage = false
    end
  end
  return State
end
  
function Program.handle_info(Request,State)
  VM.log("warning handle info at teleporter")
  return State
end

function Program.start()
  local d6 = Door.new(Bundle:new(CABLE_SIDE,colors.cyan))
  local d7 = Door.new(Bundle:new(CABLE_SIDE,colors.gray))
  local d8 = Door.new(Bundle:new(CABLE_SIDE,colors.purple))
  
  local doors = {
    Door.newUI("monitor_165","SURFACE"),
    Door.newUI("monitor_166","UTILITY"),
    Door.newUI("monitor_204","SURFACE"),
    
    Door.newUI("monitor_167","ADMIN 2",d6,455),
    Door.newUI("monitor_168","LEVEL 2",d6),
    Door.newUI("monitor_169","ADMIN 3",d7,443),
    Door.newUI("monitor_170","LEVEL 3",d7),
    
    Door.newUI("monitor_171","OFFICE"),
    Door.newUI("monitor_172","OFFICE"),
    
    Door.newUI("monitor_174","ADMIN 3",d8,443),
    Door.newUI("monitor_175"," LOBBY ",d8),
    
    Door.newUI("monitor_205","MEETING"),
    Door.newUI("monitor_177","UTILITY"),
    Door.newUI("monitor_208","SECURE"),
    Door.newUI("monitor_180","ADMIN 5"),
    Door.newUI("monitor_184","ADMIN"),
    Door.newUI("monitor_206","OFFICE"),
    
    Door.newUI("monitor_300","LEVEL 3"),
    Door.newUI("monitor_301","UTILITY"),
    Door.newUI("monitor_302","LEVEL 3"),
  }
  
  local elevator0 = callPanelUI("monitor_173",3)
  local elevator3 = callPanelUI("monitor_178",3)
  local elevator4 = callPanelUI("monitor_179",3)
  local elevator5 = callPanelUI("monitor_72",3)
  
  local arrivals1 = static_ui.startFancy("monitor_176","OPERATIONS","MEETING ROOM 305")
  local arrivals2 = static_ui.startFancy("monitor_181","OPERATIONS","BUILDING 3")
  local arrivals3 = static_ui.startFancy("monitor_183","OPERATIONS","BUILDING 4")
  
  audio_player.start_link("monitor_36","INFO","playsound fdi:event.part1.psa_welcome @a 65 47 7 1",60)
  audio_player.start_link("monitor_244","INFO","playsound fdi:event.part1.psa_map @a 86 49 -29 1",80)
  
  local lobbyDoor = Door.new(Bundle:new(CABLE_SIDE,colors.blue))
  
  local levels = {
    {
    coords={75,49,-35,2},
    level=3,
    name="Lobby",
    call="monitor_63",
    door=lobbyDoor
    }
  }
  
  local lobbyElevator = elevator.new(levels)
  elevator.newPanel("monitor_62",3,lobbyElevator)
  
  gen_server.start_link(Program,{},{})
end

return Program
