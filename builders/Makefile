.PHONY: iosevka-xy-ttf

iosevka-xy-ttf: | output
	docker build -t font-builder:latest font
	CID=$$(docker run --detach --memory=12g --memory-swap=12g font-builder:latest /scripts/build-iosevka-xy-ttf.sh) && \
		docker attach $$CID && \
		docker cp -L $$CID:/root/Iosevka-master/dist/latest.tgz output/iosevka-xy-`date +%Y%m%d`.tgz && \
		docker rm $$CID

output:
	mkdir -p output
