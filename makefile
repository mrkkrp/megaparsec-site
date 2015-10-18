#
# Some useful stuff for the site management.
#
# Call from project's root. Supported commands:
#
# - watch (default)
# - deploy
# - build
# - index
#

README="https://raw.githubusercontent.com/mrkkrp/megaparsec/master/README.md"
SITE=$(shell find ./.stack-work/dist/ -name site -type f)

.PHONY : get-index

watch : build
	$(SITE) watch

deploy : build # may be brittle
	cd ../megaparsec-gh-pages/ ; rm -vr \
	css/ js/ tutorials/ 404.html index.html tutorials.html
	cd .. ; cp -vr ./megaparsec-site/_site/* ./megaparsec-gh-pages/
	cd ../megaparsec-gh-pages/ ; git add -A ; git commit -m 'auto-sync' ; \
	git push origin gh-pages

build : index
	$(SITE) clean
	$(SITE) build

index :
	curl -Lo index-original.md $(README)
	tail -n +2 index-original.md > index-without-header.md
	cat index-header.md index-without-header.md > index.md
	rm -v index-original.md index-without-header.md
