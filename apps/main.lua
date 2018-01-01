
local Bundle = require "lib.bundle"
local Manager = require 'door_manager'
local Door = require 'door'

local Program = {}

local CABLE_SIDE = "back"
local cables = {
  entrance = Bundle:new(CABLE_SIDE,colors.black)
}

function Program.start()

  local area_1 = Door.new(Bundle:new(CABLE_SIDE,colors.white))
  local area_6 = Door.new(Bundle:new(CABLE_SIDE,colors.green))
  local area_7 = Door.new(Bundle:new(CABLE_SIDE,colors.blue))
  local area_8 = Door.new(Bundle:new(CABLE_SIDE,colors.cyan))
  local office = Door.new(Bundle:new(CABLE_SIDE,colors.black),nil,Bundle:new(CABLE_SIDE,colors.orange))
  local gym = Door.new(Bundle:new(CABLE_SIDE,colors.red),nil,Bundle:new(CABLE_SIDE,colors.magenta))
  local storage = Door.new(Bundle:new(CABLE_SIDE,colors.purple),nil,Bundle:new(CABLE_SIDE,colors.lightBlue))
  
  local doors = {
    Door.newUI("monitor_96","STORAGE"),
    Door.newUI("monitor_87","MAIN102"),
    Door.newUI("monitor_86","MAIN103"),
    Door.newUI("monitor_84","AREA 3"),
    Door.newUI("monitor_92","AREA 6"),
    Door.newUI("monitor_95","AREA 7"),
    Door.newUI("monitor_81"," LOBBY ",area_1),
    Door.newUI("monitor_82","AREA 1",area_1),
    Door.newUI("monitor_90","AREA 5",area_6),
    Door.newUI("monitor_91","AREA 6",area_6),
    Door.newUI("monitor_93","AREA 6",area_7),
    Door.newUI("monitor_94","AREA 7",area_7),
    Door.newUI("monitor_89","AREA 7",area_8),
    Door.newUI("monitor_88","AREA 8",area_8),
    Door.newUI("monitor_83","OFF.101",office,123),
    Door.newUI("monitor_85","  GYM  ",gym),
    Door.newUI("monitor_97","STORAGE",storage)
  }

  Manager.start(doors)
end

return Program