VERSION=$(shell git describe)

format:
	find lib test server daemon -name \*.hs -exec brittany --write-mode=inplace {} \;

build-image:
	docker build -t nuttycom/aftok:latest .

deploy-image: build-image
	docker tag nuttycom/aftok:latest nuttycom/aftok:$(VERSION)
	docker push docker.io/nuttycom/aftok:$(VERSION)

run-local-docker: build-container
	docker run --net=host -it -v /home/nuttycom/projects/aftok/local/conf/:/etc/aftok aftok/aftok:latest
