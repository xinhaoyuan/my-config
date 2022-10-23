.PHONY: font-builder iosevka-xy-ttf terra-builder terra

output:
	mkdir -p output

font-builder:
	docker build -t font-builder:latest font

font-iosevka-xy: docker-images | output
	CID=$$(docker run --detach --memory=12g --memory-swap=12g font-builder:latest /scripts/build-iosevka-xy.sh) && \
		docker attach $$CID && \
		( OUTPUT=output/iosevka-xy-`date +%Y%m%d`.tgz; rm $$OUTPUT 2>/dev/null; docker cp -L $$CID:/root/Iosevka-master/dist/latest.tgz $$OUTPUT ) && \
		docker rm $$CID

terra-builder:
	docker build -t terra-builder:latest terra

terra: terra-builder | output
	CID=$$(docker run --detach --memory=12g --memory-swap=12g terra-builder:latest /scripts/build-terra.sh) && \
		docker attach $$CID && echo $$CID
