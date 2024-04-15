.PHONY: shell helloworld sdl2

shell:
	docker run -it \
		--rm \
		-v ./:/source \
		-w /source \
		pspdev/pspdev:latest

helloworld:
	cd helloworld && \
	mkdir -p build && \
	cd build && \
	psp-cmake -DBUILD_PRX=1 -DENC_PRX=1 .. && \
	make

sdl2-psp:
	cd sdl2 && \
	mkdir -p psp-build && \
	cd psp-build && \
	psp-cmake -DBUILD_PRX=1 -DENC_PRX=1 .. && \
	make

sdl2:
	cd sdl2 && \
	mkdir -p build && \
	cd build && \
	cmake .. && \
	make
