local gen_server = require "gen_server"
local UI = require "lib.ui"
local Bundle = require "lib.bundle"
local Graphic = require "lib.graphic"
local Menu = require 'lib.ui_menu'
local Panel = require "lib.ui_obj"
local Door = require 'door'
local static_ui = require "static_ui"
local ui_server = require "ui_server"

local CYCLE_DELAY = 7
local DOOR_DELAY = 7
local MON_IN = "monitor_156" 
local CONTROLS = "monitor_198"

local function scanSound()
  local x,y,z = 131,72,-19
  local telesound = "/playsound fdi:event.part1.inv_scan @a[%d,%d,%d,8]"
  exec(telesound,x,y,z)
end

local function welcomeSound()
  local x,y,z = 131,72,-19
  local telesound = "/playsound fdi:event.part1.inv_arq_welcome @a[%d,%d,%d,12]"
  exec(telesound,x,y,z)
end

local Scanner = {}

local CABLE_SIDE = "right"
local cables = {
  outOpen = Bundle:new(CABLE_SIDE,colors.brown),
  outClose = Bundle:new(CABLE_SIDE,colors.blue),
  inOpen = Bundle:new(CABLE_SIDE,colors.purple),
  inClose = Bundle:new(CABLE_SIDE,colors.cyan),
  step = Bundle:new(CABLE_SIDE,colors.black),
  teethUp = Bundle:new("back",colors.red),
  teethDown = Bundle:new("back",colors.green),
  lights = Bundle:new("back",colors.white)
}

----------------
--External API--
----------------


---------------
--Server & UI--
---------------

local function securityUI(panel)
  local ui = ui_server.newWindow(CONTROLS,18,8)
  local title = Graphic:new("Airlock Controls")
  title:align("center")
  local body = Panel:new()
  body:setLayout("static")
  body.width = "max"
    
  local inButton = Graphic:new("OPEN")
  local inLabel = Graphic:new(" IN")
  local outButton = Graphic:new("OPEN")
  local outLabel = Graphic:new(" OUT")
  
  local scanButton = Graphic:new("SCAN")
  
  local spacer = Graphic:new("    ")
  spacer.ypos = 4
  
  inButton.ypos = 2
  inButton.xpos = 14
  inLabel.ypos = 3
  inLabel.xpos = 14
  
  outButton.ypos = 2
  outButton.xpos = 2
  outLabel.ypos = 3
  outLabel.xpos = 2
  
  scanButton.ypos = 2
  scanButton.xpos = 8
  
  body:add(inButton)
  body:add(inLabel)
  body:add(outButton)
  body:add(outLabel)
  body:add(scanButton)
  body:add(spacer)
  
  ui:add(title)
  ui:add(body)
    
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.orange)
  
  inButton:setOnSelect(ui,function() gen_server.cast(panel,{"inButton"})end)
  outButton:setOnSelect(ui,function() gen_server.cast(panel,{"outButton"})end)
  scanButton:setOnSelect(ui,function() gen_server.cast(panel,{"scan"})end)
  
  ui:update()
  return ui, body, inButton, outButton, scanButton
end

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
  obj.reactor:stop()
end

local function updatePanel(State)
  if State.inOpen then
    disable(State.panel,3)
    reenable(State.panel,State.In,1)
  else
    reenable(State.panel,State.Out,3)
  end
  
  if State.outOpen then
    disable(State.panel,1)
    reenable(State.panel,State.Out,3)
  else
    reenable(State.panel,State.In,1)
  end
  
  if State.inOpen then
    if State.inClosing then
      State.In.text = "Open"
    else
      State.In.text = "Close"
    end
  else
    State.In.text = "Open"
  end
  
  if State.outOpen then
--    Door.lock(State.doorIn)
    if State.outClosing then
      State.Out.text = "Open"
    else
      State.Out.text = "Close"
    end
  else
--    Door.unlock(State.doorIn)
    State.Out.text = "Open"
  end
  VM.log("In Open: "..tostring(State.inOpen).." Out Open: "..tostring(State.outOpen))
  State.ui:update()
end

function Scanner.openInner(Co)
  gen_server.cast(Co,{"open_inner"})
end

function Scanner.closeInner(Co)
  gen_server.cast(Co,{"close_inner"})
end

function Scanner.closeOuter(Co)
  gen_server.cast(Co,{"close_outer"})
end

function Scanner.openOuter(Co)
  gen_server.cast(Co,{"open_outer"})
end

function Scanner.init(drIn,drOut,teeth)
  local ui, panel, In, Out, Scan = securityUI(VM.running())
  Door.subscribe(drIn)
  Door.subscribe(drOut)
  cables.step:disable()
  local emitter = peripheral.wrap("emitter_3")
  emitter.setParticleType("fireworksSpark")
  emitter.setSpace(129,73,-23,128,72,-22)
  local emitter2 = peripheral.wrap("emitter_4")
  emitter2.setParticleType("fireworksSpark")
  emitter2.setSpace(132,73,-23,133,72,-22)
  cables.lights:disable()
  
  return true, {
    ui = ui,
    panel = panel,
    In = In,
    Out = Out,
    Scan = Scan,
    doorIn = drIn,
    doorOut = drOut,
    teeth = teeth,
    inOpen = false,
    inOpening = false,
    inClosing = false,
    outOpen = false,
    outOpening = false,
    outClosing = false,
    scanning = false,
    emitter = emitter,
    emitter2 = emitter2,
    lights = cables.lights
    }
end

local function runScanner(emitter,emitter2,lights)
    scanSound()
    EVE.sleep(7)
    lights:enable()
    emitter.setEmitting(true)
    emitter2.setEmitting(true)
    emitter.setRate(2)
    emitter2.setRate(2)
    EVE.sleep(6.5)
    emitter.setEmitting(false)
    emitter2.setEmitting(false)
    EVE.sleep(2)
    lights:disable()
    gen_server.cast("scanner",{"done_scan"})
end

function Scanner.handle_call(Request,From,State)
  local event = Request[1]
  VM.log("Got "..event)
  
  return State
end

function Scanner.handle_cast(Request,State)
  local event = Request[1]
  VM.log("Scanner got "..event)
  if event == "inButton" then
    if State.inOpen then
      if Door.close(State.doorIn) == "closed" then
        State.ui:tap()
      else
        State.ui:beep()
      end
    else
      if State.outOpen then
        State.ui:beep()
      else
        if Door.open(State.doorIn) == "opened" then
          State.ui:ping()
        else
          State.ui:beep()
        end
      end
    end  
  elseif event == "outButton" then
    if State.outOpen then
      if Door.close(State.doorOut) == "closed" then
--        Door.close(State.teeth)
        State.ui:tap()
      end
    else
      if State.inOpen then
        State.ui:beep()
      else
        if Door.open(State.doorOut) == "opened" then
          if not State.scanning then
            welcomeSound()
          end
          VM.log(tostring(VM.running()).. " requested teeth open to door: "..tostring(State.teeth))
--          Door.open(State.teeth)
          State.ui:ping()
        end
      end
    end
  elseif event == "opened" then
    local _,door = unpack(Request)
    if door == State.doorIn then
      State.inOpen = true
      State.inOpening = false
      State.inClosing = false
    else
      State.outOpen = true
      State.outOpening = false
      State.outClosing = false
    end
    updatePanel(State)
  elseif event == "closed" then
    local _,door = unpack(Request)
    if door == State.doorIn then
      State.inOpen = false
      State.inOpening = false
      State.inClosing = false
    else
      State.outOpen = false
      State.outOpening = false
      State.outClosing = false
    end
    updatePanel(State)
  elseif event == "opening" then
    local _,door = unpack(Request)
    if door == State.doorIn then
      State.inOpening = true
      State.inOpen = true
      State.inClosing = false
    else
      State.outOpening= true
      State.outOpen = true
      State.outClosing = false
    end
    updatePanel(State)
  elseif event == "closing" then
    local _,door = unpack(Request)
    if door == State.doorIn then
      State.inOpen = true
      State.inClosing = true
      State.inOpening = false
    else
      State.outOpen = true
      State.outClosing = true
      State.outOpening = false
    end
    updatePanel(State)
  elseif event == "scan" then
    if not State.scanning then
      State.scanning = true
      VM.spawnlink(function() runScanner(State.emitter,State.emitter2,State.lights) end)
      State.ui:ping()
      updatePanel(State)
    end
  elseif event == "done_scan" then
    State.scanning = false
    updatePanel(State)
  end
  return State
end

function Scanner.handle_info(Request,State)
  VM.log("warning handle info at scanner")
  return State
end

function Scanner.start()
  local doorIn = Door.newCargo(cables.inOpen,cables.inClose,DOOR_DELAY)
  local doorOut = Door.newCargo(cables.outOpen,cables.outClose,DOOR_DELAY)
  local teeth = Door.newCargo(cables.teethUp,cables.teethDown,5)
  local access = Door.newUI("monitor_156","AIRLOCK",doorIn)
  local arrival = gen_server.start_link(Scanner,{doorIn,doorOut,teeth},{},"scanner")
end

return Scanner