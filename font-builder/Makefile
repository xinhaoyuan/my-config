.PHONY: iosevka-xy-ttf

iosevka-xy-ttf:
	docker build -t font-builder:latest .
	CID=$$(docker run --detach font-builder:latest /scripts/build-iosevka-xy-ttf.sh) && docker attach $$CID && docker cp $$CID:/root/iosevka/dist/iosevka-xy iosevka-xy
