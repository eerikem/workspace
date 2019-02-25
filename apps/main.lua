local Bundle = require "lib.bundle"
local Door = require 'door'
local teleUI = require 'telehub_ui'
local Teleporter = require 'telehub'


-- The side which a redstone cable is connected
-- Replace nil with "back", "left", "right", "top" or "bottom"
local CABLE_SIDE = "left"


local App = {}

----------------------
--UI Client Template--
----------------------
--
local UI = require "lib.ui"


local fan = Bundle:new(CABLE_SIDE,colors.red)
local lights = {
  overhead = Bundle:new(CABLE_SIDE,colors.lightGray),
  walkway = Bundle:new(CABLE_SIDE,colors.orange),
  vent = Bundle:new(CABLE_SIDE,colors.cyan),
}

local Client = {}
---
-- @param #string mon the monitor to spawn the ui
-- @return #thread address to new UI Client
function Client.start_link(mon)
  local Graphic = require 'lib.graphic'
  --- init function to initialize Client UI.
  -- @function [parent=#ui_client] init
  -- @param lib.ui_lib#ui ui The ui object initialized in UI.lua
  local function init(ui)
    VM.log(string.format("term has dimensions %d %d",ui.term.getSize()))
    local title = Graphic:new("VENTILATION  PANEL")
    local enableLights =  "Enable  Lighting"
    local enableFans =    "Enable  Fans    "
    local disableLights = "Disable Lighting"
    local disableFans =   "Disable Fans    "
    local lightsButton = Graphic:new(enableLights)
    local fans = Graphic:new(enableFans) 
    
    local fansOn = false
    local lightsOn = false
    
    lightsButton:setOnSelect(ui,function()
      if lightsOn then
        lightsButton.text = enableLights
        for k,v in pairs(lights) do
          v:disable()
        end
        lightsOn = false
        ui:tap()
      else
        lightsButton.text = disableLights
        for k,v in pairs(lights) do
          v:enable()
        end
        lightsOn = true
        ui:ping()
      end
      ui:update()
    end)
    fans:setOnSelect(ui,function()
      if fansOn then
        fans.text = enableFans
        fan:disable()
        fansOn = false
        ui:tap()
      else
        fans.text = disableFans
        fan:enable()
        fansOn = true
        ui:ping()
      end
      ui:update()
    end)
    
    
    ui:add(title)
    lightsButton.ypos = 2
    lightsButton:align("center")
    fans:align("center")
    ui:add(lightsButton)
    ui:add(fans)
    
    local function bright()
      title:setBackgroundColor(colors.lightGray)
      title:setTextColor(colors.gray)
      ui:setBackground(colors.gray)
      ui:setText(colors.orange)
    end
    
    local function dark()
      title:setBackgroundColor(colors.gray)
      title:setTextColor(colors.black)
      ui:setBackground(colors.black)
      ui:setText(colors.white)
    end
    
--    bright()
    dark()
    
    ui:update()
  end
  
  return UI.start(mon,18,5,init)
end



local Alarm = {}
---
-- @param #string mon the monitor to spawn the ui
-- @return #thread address to new UI Client
function Alarm.start_link(mon,alarm)
  local Graphic = require 'lib.graphic'
  local Panel = require 'lib.ui_obj'
  --- @param lib.ui_lib#ui ui The ui object initialized in UI.lua
  local function init(ui)
    VM.log(string.format("term has dimensions %d %d",ui.term.getSize()))
    local title = Graphic:new(" ALARM ")
    local enableLights =  "PANIC"
    local disableLights = "SECURED"
    local lightsButton = Graphic:new(enableLights) 
    local lightsOn = false
    local body = Panel:new()
    body.width = "max"
    lightsButton:setOnSelect(ui,function()
      if lightsOn then
        lightsButton.text = enableLights
        lightsButton:setTextColor(colors.red)
        alarm:disable()
        lightsOn = false
        ui:tap()
      else
        lightsButton.text = disableLights
        lightsButton:setTextColor(colors.orange)
        alarm:enable()
        lightsOn = true
        ui:ping()
      end
      ui:update()
    end)
    
    ui:add(title)
    lightsButton.ypos = 2
    lightsButton:align("center")
    body:add(lightsButton)
    body:setHeight(3)
    ui:add(body)
    
    local function bright()
      body:setBackgroundColor(colors.gray)
      body:setTextColor(colors.red)
      ui:setBackground(colors.lightGray)
      ui:setText(colors.gray)
    end
    
    local function dark()
      title:setBackgroundColor(colors.gray)
      title:setTextColor(colors.black)
      ui:setBackground(colors.black)
      ui:setText(colors.white)
    end
    
    bright()
--    dark()
    
    ui:update()
  end
  
  return UI.start(mon,7,5,init)
end




------------
--  Main  --
------------

---
-- start() is called when this file is saved in the /apps directory
-- Intended to be used as a start up script for a particular arq instance
function App.start()

  local hub1 = Teleporter.newProps({-110,59,-5},{4,18,46,0},"emitter_1")
  hub1.door = Door.new(Bundle:new(CABLE_SIDE,colors.gray))
  hub1.detector = Bundle:new(CABLE_SIDE,colors.magenta)
  hub1.light = Bundle:new(CABLE_SIDE,colors.lime)
  
  local tele1 = Teleporter.start(hub1)
--  local tele2 = Teleporter.start(hub2)
  
  EVE.sleep(0.5)
    
  local static_ui = require "static_ui"
  local Airlock = require "airlock"
  local Lift = require 'lift'
  
  local storage = Door.new(Bundle:new(CABLE_SIDE,colors.black))
  local office = Door.new(Bundle:new(CABLE_SIDE,colors.blue),5,Bundle:new(CABLE_SIDE,colors.white))
  
  Door.newUI("monitor_249","HAZARD")
  Door.newUI("monitor_250","LEVEL 1")
  Door.newUI("monitor_253","LOBBY")
  Door.newUI("monitor_252","OFFICE",office,455)
  Door.newUI("monitor_255","AIRLOCK")
  Door.newUI("monitor_257","LEVEL 2")
  
  Door.newUI("monitor_303","STORAGE",storage)
  Door.newUI("monitor_304","LEVEL 2",storage)
  
  local sectorB = static_ui.startFancy("monitor_256","SECTOR E TELEHUB","ONLINE")
  
  local props = {
    middle = "monitor_258",
    doors = {
      outer = Bundle:new(CABLE_SIDE,colors.yellow,"monitor_259"),
      inner = Bundle:new(CABLE_SIDE,colors.lightBlue,"monitor_260"),
    },
  }
  Airlock.start(props)
  
  teleUI.start("monitor_261","TELEHUB",tele1)
  teleUI.start("monitor_262","TELEHUB")
  
  local elevator = Lift.new(
    Bundle:new(CABLE_SIDE,colors.green),
    Bundle:new(CABLE_SIDE,colors.brown),
    30)
  Lift.newUI("monitor_263",elevator,"LIFT")
  Lift.newUI("monitor_265",elevator,"LIFT")
  
  Client.start_link('monitor_266')
  Alarm.start_link("monitor_251",Bundle:new(CABLE_SIDE,colors.pink))
  
end

return App