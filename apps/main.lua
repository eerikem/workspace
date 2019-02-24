local Bundle = require "lib.bundle"
local Airlock = require 'airlock'
local Door = require 'door'
local static_ui = require "static_ui"
--local audio_player = require "audio_player"
local elevator = require 'elevator'
local Teleporter = require "telehub"
local teleUI = require "telehub_ui"
local teleInfo = require "telehub_info"
---
-- The Telehub Computer

local CABLE_SIDE = "left"

local function teleArriveSound(x,y,z)
  local telesound = "/playsound fdi:event.teleport_general_arrive @a[%d,%d,%d,20]"
  exec(telesound,x,y,z)
end

local function teleLeaveSound(x,y,z)
  local telesound = "/playsound fdi:event.teleport_general @a[%d,%d,%d,20]"
  exec(telesound,x,y,z)
end

local Program = {}

function Program.start()
  local lvl2 = Door.new(Bundle:new(CABLE_SIDE,colors.pink))
  local doors = {
    Door.newUI("monitor_289","LEVEL 2",lvl2,455),
    Door.newUI("monitor_290","LEVEL 2",lvl2),
    Door.newUI("monitor_307","UTILITY"),
    Door.newUI("monitor_292","UTILITY"),
  }
    
  local sectorB = static_ui.startFancy("monitor_291","SECTOR B","DEP. OF INNOVATION")
  local security = static_ui.startFancy("monitor_293","SECTOR B LEVEL 6","HIGH SECURITY")
    
  local sectE = Teleporter.newProps({18,46,0},{9,-111,59,-6},"emitter_6")
  sectE.door = Door.new(Bundle:new(CABLE_SIDE,colors.white))
  sectE.detector = Bundle:new(CABLE_SIDE,colors.magenta)
  sectE.light = Bundle:new(CABLE_SIDE,colors.orange)
  
  local dorm = Teleporter.newProps({48,46,-16},{4,72,25,-7},"emitter_7")
  dorm.door = Door.new(Bundle:new(CABLE_SIDE,colors.lightBlue))
  dorm.detector = Bundle:new(CABLE_SIDE,colors.lime)
  dorm.light = Bundle:new(CABLE_SIDE,colors.yellow)
  
  local teleSecE = Teleporter.start(sectE)
  local teleDorm2 = Teleporter.start(dorm)
  
  teleUI.start("monitor_281","SECT. E",teleSecE)
  teleUI.start("monitor_282","DORM. 2",teleDorm2)
  teleUI.start("monitor_283","DORM. 3")
  teleUI.start("monitor_284","DORM. 1")
  teleUI.start("monitor_285","SECT. A")
  teleUI.start("monitor_286","SECT. D")
  teleUI.start("monitor_287","SECT. C")
  teleUI.start("monitor_288","SECT. B")
  
  teleInfo.start("monitor_297",{{"DORM.1","Offline"},{"SECT.A","Offline"},{"SECT.D","Offline"},{"SECT.E","Online"}})
  teleInfo.start("monitor_296",{{"DORM.1","Offline"},{"SECT.A","Offline"},{"SECT.D","Offline"},{"SECT.E","Online"}})
  teleInfo.start("monitor_298",{{"DORM.2","Online"},{"DORM.3","Offline"},{"SECT.B","Offline"},{"SECT.C","Offline"}})
  teleInfo.start("monitor_299",{{"DORM.2","Online"},{"DORM.3","Offline"},{"SECT.B","Offline"},{"SECT.C","Offline"}})

end

return Program
