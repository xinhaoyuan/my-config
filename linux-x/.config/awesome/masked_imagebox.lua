return function (ib)
    function ib:draw(_, cr, width, height)
        if width == 0 or height == 0 or not self._private.default then return end

        -- Set the clip
        if self._private.clip_shape then
            cr:clip(self._private.clip_shape(cr, width, height, unpack(self._private.clip_args)))
        end

        if not self._private.resize_forbidden then
            -- Let's scale the image so that it fits into (width, height)
            local w, h = self._private.default.width, self._private.default.height
            local aspect = math.min(width / w, height / h)
            cr:scale(aspect, aspect)
        end

        if self._private.image then
            cr:mask_surface(self._private.image, 0, 0)
        end
    end
    return ib
end
