for f in `find -name "*.lua"`; do
    echo $f
    luac -l -p $f | grep ENV | egrep -v '"debug"|"os"|"require"|"ipairs"|"pairs"|"io"|"string"|"table"|"math"|"tonumber"|"tostring"|"dofile"|"type"|"setmetatable"|"assert"|"print"' 
done

