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
  
  local teleDoorSecE = Door.new(Bundle:new(CABLE_SIDE,colors.white))
  Door.lock(teleDoorSecE)

  local teleDoorDorm = Door.new(Bundle:new(CABLE_SIDE,colors.lightBlue))
  Door.lock(teleDoorDorm)
  
  local emitter6 = peripheral.wrap("emitter_6")
  local x,y,z=18,46,0
  emitter6.setSpace(x-1,y,z-1,x+1,y+2,z+1)

  local teleSecE = Teleporter.start(
    function() teleLeaveSound(x,y+3,z) end,
    function() exec("tpx @a[x=%d,y=%d,z=%d,r=1] 4 26 45 0",x,y,z) end,
    8,10,
    teleDoorSecE,
    Bundle:new(CABLE_SIDE,colors.magenta),
    Bundle:new(CABLE_SIDE,colors.orange),
    emitter6
  )
  
  local emitter7 = peripheral.wrap("emitter_7")
  local a,b,c=48,46,-16
  emitter7.setSpace(a-1,b,c-1,a+1,b+2,c+1)
  local teleDorm2 = Teleporter.start(
    function() teleLeaveSound(a,b,c) end,
    function() exec("tpx @a[x=%d,y=%d,z=%d,r=1] 4 39 45 -16",a,b,c) end,
    8,10,
    teleDoorDorm,
    Bundle:new(CABLE_SIDE,colors.lime),
    Bundle:new(CABLE_SIDE,colors.yellow),
    emitter7
  )
  
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
