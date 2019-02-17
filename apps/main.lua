local gen_server = require "gen_server"
local UI = require "lib.ui"
local Bundle = require "lib.bundle"
local Graphic = require "lib.graphic"
local Menu = require 'lib.ui_menu'
local Panel = require "lib.ui_obj"
local Airlock = require 'airlock'
local Door = require 'door'
local static_ui = require "static_ui"
--local audio_player = require "audio_player"
local elevator = require 'elevator'

---
-- The Telehub Computer
local Program = {}

local CABLE_SIDE = "left"
local cables = {

}
local function teleArriveSound()
  local x,y,z = 134,71,10
  local telesound = "/playsound fdi:event.teleport_general_arrive @a[%d,%d,%d,20]"
  exec(telesound,x,y,z)
end

local function teleLeaveSound()
local x,y,z = 134,71,10
  local telesound = "/playsound fdi:event.teleport_general @a[%d,%d,%d,20]"
  exec(telesound,x,y,z)
end
---------------
--Server & UI--
---------------

---
-- @type Teleporter
-- @extends gen_server#server
local Teleporter = {}

---
-- @param #function callback The function to execute when teleport is called
-- @param #thread door optional door of teleport bay
function Teleporter.init(callback,door)
  local State = {
    callback = callback,
    door = door,
    teleporting=false,
    primed=false,
    subscribers = {},
  }
  return true, State
end

-- TODO why does gen_server cast fail when message is not in a table
function Teleporter.queueTransit(Co)
  gen_server.cast(Co,{"queue_transit"})
end


--TODO standardise this behaviour in a super class
local function notify(State,event,...)
  for Co,_ in pairs(State.subscribers) do
    gen_server.cast(Co,{event,unpack(arg)})
  end
end

function Teleporter.handle_cast(Request,State)
  local event = Request[1]
  if event == "queue_transit" then
    VM.log("Teleporter queueing transit")
    Door.open(State.door)
  elseif event == "subscribe" then
    local _,Co = unpack(Request)
    State.subscribers[Co]=true
  end
  return State
end

function Teleporter.subscribe(teleporter,co)
  if not teleporter then error("badarg",2) end
  local co = co or VM.running()
  gen_server.cast(teleporter,{"subscribe",co})
end

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

local TeleUI = {}

---
-- @param #string monitor
-- @param #string destination
-- @param #thread teleporter optional coroutine
function TeleUI.start_link(monitor,destination,teleCo)
---
-- @param lib.ui_lib#ui ui
local function initTeleUI(ui)
    local title = Graphic:new(destination)
    local body = Panel:new()
    local open = Graphic:new(" QUEUE TRANSIT")
    local close = Graphic:new("TELEHUBPrimed ")
    local teleport = Graphic:new("SendingParcels")
    local arrived = Graphic:new("RebuiltArrival")
    local success = Graphic:new("TRANSITSUCCESS")
    local denied = Graphic:new("Denied!")
    local cycler = Graphic:new("       ")
    body.width = "max"
    open.ypos = 2
    close.ypos = 2
--    cycler.ypos = 3
    ui:add(title)
    body:add(open)
    body:add(cycler)
    ui:add(body)
    close.reactor:stop()
    teleport.reactor:stop()
    arrived.reactor:stop()
    success.reactor:stop()
    --TODO Remove Duplicate Hacks
    local function enable(button2)
      local button1 = body.index[1]
      if button1 ~= button2 then
        body:replace(button1,button2)
        button1.reactor:stop()
        button2.reactor:start()
        ui:update()
      end
      return button1
    end
    
    local function deny()
      ui:beep()
      local prevButton = enable(denied)
      ui:update()
      EVE.sleep(1)
      enable(prevButton)
    end
    
    open:setOnSelect(ui,function()
      ui:tap()
      Teleporter.queueTransit(teleCo)
    end)
    
    local function bright()
      ui:setBackground(colors.lightGray)
      ui:setText(colors.gray)
      body:setTextColor(colors.orange)
      body:setBackgroundColor(colors.gray)
      denied:setTextColor(colors.red)
    end

    bright()
    ui:update()
    if teleport then
      Teleporter.subscribe(teleCo)
    end
end

  return UI.start(monitor,7,5,initTeleUI)
end

local function initUI(monitor)
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

function Program.init(door)
  local overheadLights = Bundle:new("back",colors.lightGray)
  overheadLights:enable()
  Door.
  Door.subscribe(door)
  return true, {
    door = door,
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
  if event == "opened" then
    Door.lock(State.door)
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

--  local lobbyDoor = Door.new(Bundle:new(CABLE_SIDE,colors.black),10)
--  
--  local levels = {
--    {
--    level=3,
--    name="Dest",
----    callback = function() Door.open(lobbyDoor) end,
----    callback = function() exec("tpx @a[x=75,y=49,z=-35,r=2] 4 58 31 -35") end,
----    callback = function() exec("tpx @a[x=58,y=31,z=-34,r=2] 3 75 49 -36") end,
--    callback = function() exec("tpx @a[x=58,y=31,z=-34,r=2] 3 ~17 ~18 ~-2") end,
----    callback = function() exec("say @a Elevator arrived at level 1") end,
--    },
--    {
--    coords={75,49,-35,2},
--    level=1,
--    name="Lobby",
--    call="monitor_233",
--    door=lobbyDoor
--    },
--  }
--  
  local lvl2 = Door.new(Bundle:new(CABLE_SIDE,colors.pink))
  local doors = {
    Door.newUI("monitor_289","LEVEL 2",lvl2,455),
    Door.newUI("monitor_290","LEVEL 2",lvl2),
    Door.newUI("monitor_307","UTILITY"),
    Door.newUI("monitor_292","UTILITY"),
  }
    
  local sectorB = static_ui.startFancy("monitor_291","SECTOR B","DEP. OF INNOVATION")
  local security = static_ui.startFancy("monitor_293","SECTOR B LEVEL 6","HIGH SECURITY")
  
  local teleDoorSecE = Door.new(Bundle:new(CABLE_SIDE,colors.white))
--  Door.newUI("monitor_286","TeleHub",teleDoorSecE)
  local ok, teleSecE = gen_server.start_link(Teleporter,{function()VM.log("Received Request to teleport")end,teleDoorSecE})
  TeleUI.start_link("monitor_281","SECT. E",teleSecE)
--  audio_player.start_link("monitor_36","INFO","playsound fdi:event.part1.psa_welcome @a 65 47 7 1",60)
      
--  gen_server.start_link(Program,{comp},{})
end

return Program
