-- from http://lua-users.org/wiki/TableUtils

function table.val_to_str ( v )
   if "string" == type( v ) then
      v = string.gsub( v, "\n", "\\n" )
      if string.match( string.gsub(v,"[^'\"]",""), '^"+$' ) then
         return "'" .. v .. "'"
      end
      return '"' .. string.gsub(v,'"', '\\"' ) .. '"'
   else
      return "table" == type( v ) and table.tostring( v ) or
         tostring( v )
   end
end

function table.key_to_str ( k )
   if "string" == type( k ) and string.match( k, "^[_%a][_%a%d]*$" ) then
      return k
   else
      return "[" .. table.val_to_str( k ) .. "]"
   end
end

function table.tostring( tbl )
   local result, done = {}, {}
   for k, v in ipairs( tbl ) do
      table.insert( result, table.val_to_str( v ) )
      done[ k ] = true
   end
   for k, v in pairs( tbl ) do
      if not done[ k ] then
         table.insert( result,
                       table.key_to_str( k ) .. "=" .. table.val_to_str( v ) )
      end
   end
   return "{" .. table.concat( result, "," ) .. "}"
end

function is_tiling(c)
   return
      not (c.tomb_floating or c.floating or c.maximized_horizontal or c.maximized_vertical or c.maximized or c.fullscreen)
end

function set_tiling(c)
   c.floating = false
   c.maximized = false
   c.maximized_vertical = false
   c.maximized_horizontal = false
   c.fullscreen = false
end

return {
   is_tiling = is_tiling,
   set_tiling = set_tiling,
}
