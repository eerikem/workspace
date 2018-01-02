local gen_server = require "gen_server"
local UI = require "lib.ui"
local Bundle = require "lib.bundle"
local Graphic = require "lib.graphic"
local Menu = require 'lib.ui_menu'
local Panel = require "lib.ui_obj"
local Airlock = require 'airlock'
local Door = require 'door'
local static_ui = require "static_ui"

local Program = {}

local CABLE_SIDE = "back"
local cables = {
  teleUp = Bundle:new(CABLE_SIDE,colors.orange),
  teleDn = Bundle:new(CABLE_SIDE,colors.white),
  teleLight = Bundle:new(CABLE_SIDE,colors.brown),
  teleRoom = Bundle:new(CABLE_SIDE,colors.lightGray)
}

local function teleSound()
  local x,y,z = -38,69,173
  local telesound = "/playsound fdi:event.part0.teleport_overworld @a[%d,%d,%d,16]"
  exec(telesound,x,y,z)
end

function Program.teleport()
  return gen_server.call("teleporter",{"teleport"})
end

function Program.openSilo()
  return gen_server.call("teleporter",{"open_silo"})
end

function Program.closeSilo()
  gen_server.cast("teleporter",{"close_silo"})
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

local function runTeleporter(emitter)
    teleSound()
    cables.teleRoom:disable()
    EVE.sleep(1)
    
    emitter.setParticleType("portal")
    emitter.setEmitting(true)
    emitter.setRate(0.3)
    EVE.sleep(6)
    emitter.setRate(2)
    EVE.sleep(2)
    emitter.setRate(5)
    EVE.sleep(2.5)
    emitter.setParticleType("spell")
    EVE.sleep(1.5)
    emitter.setEmitting(false)
    EVE.sleep(1)
    cables.teleRoom:enable()
    gen_server.cast("teleporter",{"done_teleport"})
end

function Program.init(monitor)
  cables.teleRoom:enable()
  
  local control = Door.new(Bundle:new(CABLE_SIDE,colors.pink))
  local teleporter = Door.newCargo(cables.teleUp,cables.teleDn,7.5)
  Door.subscribe(teleporter)
  local ui = initUI(monitor)
  
  local emitter = peripheral.wrap("emitter_0")
  emitter.setParticleType("portal")
  emitter.setSpace(-38,67,180,-40,69,178)
    
  local doors = {
--    Door.newUI("monitor_194","Teleporter Bay",teleporter),
    Door.newUI("monitor_79","CONTROL",control),
    Door.newUI("monitor_80"," LOBBY ",control)
  }
  
--  Door.close(teleporter)
--  EVE.sleep(3)
--  emit()
  
  local arrivals = static_ui.startFancy("monitor_77","FDI TELEPORTER","End Arrivals")
  local departures = static_ui.startFancy("monitor_78","FDI TELEPORTER","End Departures")
  
  return true, {
    ui = ui,
    teleporter = teleporter,
    teleporting = false,
    emitter = emitter,
    siloOpen = true
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
  end
  return State
end
  
function Program.handle_info(Request,State)
  VM.log("warning handle info at teleporter")
  return State
end

function Program.start()
  local co = gen_server.start(Program,{"monitor_194"},{},"teleporter")
  Program.openSilo()
end

return Program