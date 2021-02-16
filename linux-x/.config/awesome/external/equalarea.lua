local math     = math
local screen   = screen
local tonumber = tonumber
local equalarea = {name = "equalarea"}
local function divide(p,g,low,high,cls,mwfact,mcount)
  if low == high then
    p.geometries[cls[low]] = g
  else
    local masters = math.max(0,math.min(mcount,high)-low+1)
    local numblock = high-low + 1
    local slaves = numblock - masters
    local smalldiv = math.floor((slaves+mwfact*masters)/2)
    local bigdiv = numblock - smalldiv
    local bigmasters = math.min(masters,bigdiv)
    -- local smallmasters = masters-bigmasters
    local smallg = {}
    local bigg = {}
    bigg.x = g.x
    bigg.y = g.y
    if g.width > (g.height*1.2) then
      smallg.height = g.height
      bigg.height = g.height
      bigg.width = math.floor(g.width*(bigmasters*(mwfact-1)+bigdiv)/(slaves+mwfact*masters))
      smallg.width = g.width-bigg.width
      smallg.y = g.y
      smallg.x = g.x + bigg.width
    else
      smallg.width = g.width
      bigg.width = g.width
      bigg.height = math.floor(g.height*(bigmasters*(mwfact-1)+bigdiv)/(slaves+mwfact*masters))
      smallg.height = g.height-bigg.height
      smallg.x = g.x
      smallg.y = g.y + bigg.height
    end
    divide(p,bigg,low,high-smalldiv,cls,mwfact,mcount)
    divide(p,smallg,low+bigdiv,high,cls,mwfact,mcount)
  end
  return
end

local function arrange(p)
  local t   = p.tag or screen[p.screen].selected_tag
  local wa  = p.workarea
  local cls = p.clients

  if #cls == 0 then return end
  local mwfact          = t.master_width_factor*2
  local mcount = t.master_count
  local g = {}
  g.height = wa.height
  g.width = wa.width
  g.x = wa.x
  g.y = wa.y
  divide(p,g,1,#cls,cls,mwfact,mcount)
end

function equalarea.arrange(p)
  return arrange(p)
end

return equalarea
