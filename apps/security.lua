local gen_server = require "gen_server"
local ui_server = require "ui_server"
local Bundle = require "lib.bundle"
local Door = require 'door'
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"

local Security = {}

local CABLE_SIDE = "back"
local cables = {
  depIn = Bundle:new(CABLE_SIDE,colors.lightBlue),
  depOut = Bundle:new(CABLE_SIDE,colors.magenta),
  arrIn = Bundle:new(CABLE_SIDE,colors.yellow),
  arrOut = Bundle:new(CABLE_SIDE,colors.lime)
}

 local function securityUI(monitor,title,panel)
  local ui = ui_server.newWindow(monitor,18,8)
  local title = Graphic:new(title)
  title:align("center")
  local body = Panel:new()
  body:setLayout("static")
  body.width = "max"
  
  local inButton = Graphic:new("OPEN")
  local inLabel = Graphic:new(" IN")
  local outButton = Graphic:new("OPEN")
  local outLabel = Graphic:new(" OUT")
  
  local spacer = Graphic:new("    ")
  spacer.ypos = 4
  
  inButton.ypos = 2
  inButton.xpos = 13
  inLabel.ypos = 3
  inLabel.xpos = 13
  
  outButton.ypos = 2
  outButton.xpos = 3
  outLabel.ypos = 3
  outLabel.xpos = 3
  
  body:add(inButton)
  body:add(inLabel)
  body:add(outButton)
  body:add(outLabel)
  body:add(spacer)
  
  ui:add(title)
  ui:add(body)
    
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.orange)
  
  inButton:setOnSelect(ui,function() gen_server.cast(panel,{"inButton"})end)
  outButton:setOnSelect(ui,function() gen_server.cast(panel,{"outButton"})end)
  
  ui:update()
  return ui, body, inButton, outButton
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
    State.In.text = "Close"
    State.Out.text = "Open"
    disable(State.panel,3)
    reenable(State.panel,State.In,1)
  elseif State.outOpen then
    State.In.text = "Open"
    State.Out.text = "Close"
    disable(State.panel,1)
    reenable(State.panel,State.Out,3)
  else
    State.In.text = "Open"
    State.Out.text = "Open"
    reenable(State.panel,State.In,1)
    reenable(State.panel,State.Out,3)
  end
  State.ui:update()
end

function Security.openInner(Co)
  gen_server.cast(Co,{"open_inner"})
end

function Security.closeInner(Co)
  gen_server.cast(Co,{"close_inner"})
end

function Security.closeOuter(Co)
  gen_server.cast(Co,{"close_outer"})
end

function Security.openOuter(Co)
  gen_server.cast(Co,{"open_outer"})
end

function Security.init(monitor,title,drIn,drOut)
  local ui, panel, In, Out = securityUI(monitor,title,VM.running())
  Door.subscribe(drIn)
  Door.subscribe(drOut)
  return true, {
    ui = ui,
    panel = panel,
    In = In,
    Out = Out,
    doorIn = drIn,
    doorOut = drOut,
    inOpen = false,
    outOpen = false
    }
end

function Security.handle_call(Request,From,State)
  local event = Request[1]
  VM.log("Got "..event)
  
  return State
end

function Security.handle_cast(Request,State)
  local event = Request[1]
  VM.log("Got "..event)
  if event == "inButton" then
    if State.inOpen then
      if Door.close(State.doorIn) == "closed" then
        State.ui:tap()
      end
    else
      if State.outOpen then
        State.ui:beep()
      else
        if Door.open(State.doorIn) == "opened" then
          State.ui:ping()
        end
      end
    end  
  elseif event == "outButton" then
    if State.outOpen then
      if Door.close(State.doorOut) == "closed" then
        State.ui:tap()
      end
    else
      if State.inOpen then
        State.ui:beep()
      else
        if Door.open(State.doorOut) == "opened" then
          State.ui:ping()
        end
      end
    end
  elseif event == "opened" then
    local _,door = unpack(Request)
    if door == State.doorIn then
      State.inOpen = true
    else
      State.outOpen = true
    end
    updatePanel(State)
  elseif event == "closed" then
    local _,door = unpack(Request)
    if door == State.doorIn then
      State.inOpen = false
    else
      State.outOpen = false
    end
    updatePanel(State)
  end
  return State
end

function Security.handle_info(Request,State)
  VM.log("warning handle info at security")
  return State
end

function Security.start()
  local depIn = Door.new(cables.depIn,10)
  local depOut =  Door.new(cables.depOut,10)
  local arrIn = Door.new(cables.arrIn,10)
  local arrOut =  Door.new(cables.arrOut,10)
  
  local departure = gen_server.start_link(Security,{"monitor_195","DEPARTURE CONTROLS",depIn,depOut},{})
  local arrival = gen_server.start_link(Security,{"monitor_196","ARRIVALS CONTROL",arrIn,arrOut},{})
    
end

return Security