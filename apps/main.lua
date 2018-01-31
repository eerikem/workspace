
local Bundle = require "lib.bundle"
local Airlock = require 'airlock'
local Door = require 'door'

local Program = {}

local CABLE_SIDE = "back"
local cables = {
  entrance = Bundle:new(CABLE_SIDE,colors.black)
}

function Program.start()
  VM.log("Running Airlock")
  Airlock.start()
  local entrance = Door.new(cables.entrance,8)
  Door.newUI("monitor_190","ENTRY",entrance,123)
  Door.newUI("monitor_191","HELIPAD",entrance)

end

return Program