local Bundle = require "lib.bundle"
local Airlock = require 'airlock'
local Door = require 'door'
local static_ui = require "static_ui"
--local audio_player = require "audio_player"
local elevator = require 'elevator'
local Teleporter = require "telehub"
local teleUI = require "telehub_ui"
---
-- The Telehub Computer

local CABLE_SIDE = "left"

---------------
--Server & UI--
---------------


------------
--  Main  --
------------

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
--  Door.newUI("monitor_286","TeleHub",teleDoorSecE)
  local teleSecE = Teleporter.start(
    function() exec("tpx @a[x=18,y=46,z=0,r=1] 4 26 45 0") end,
    teleDoorSecE,
    Bundle:new(CABLE_SIDE,colors.magenta),
    Bundle:new(CABLE_SIDE,colors.orange)
  )
  
  teleUI.start("monitor_281","SECT. E",teleSecE)
--  audio_player.start_link("monitor_36","INFO","playsound fdi:event.part1.psa_welcome @a 65 47 7 1",60)
      
--  gen_server.start_link(Program,{comp},{})
end

return Program
