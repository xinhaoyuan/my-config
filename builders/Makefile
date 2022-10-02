.PHONY: docker-images iosevka-xy-ttf

docker-images:
	docker build -t font-builder:latest font

font-iosevka-xy: docker-images | output
	CID=$$(docker run --detach --memory=12g --memory-swap=12g font-builder:latest /scripts/build-iosevka-xy.sh) && \
		docker attach $$CID && \
		( OUTPUT=output/iosevka-xy-`date +%Y%m%d`.tgz; rm $$OUTPUT 2>/dev/null; docker cp -L $$CID:/root/Iosevka-master/dist/latest.tgz $$OUTPUT ) && \
		docker rm $$CID

output:
	mkdir -p output
