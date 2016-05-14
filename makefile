#
# Some useful stuff for the site management.
#
# Call from project's root. Supported commands:
#
# - watch (default)
# - deploy
# - build
# - gh-pages
# - index

GH_PAGES="https://github.com/mrkkrp/megaparsec.git"
README="https://raw.githubusercontent.com/mrkkrp/megaparsec/master/README.md"

.PHONY : get-index

watch : build
	stack exec site watch

deploy : build gh-pages # may be brittle
	cd megaparsec-gh-pages/ ; rm -vfr \
	css/ js/ tutorials/ 404.html index.html tutorials.html
	cp -vr _site/* megaparsec-gh-pages/
	cd megaparsec-gh-pages/ ; git add -A ; git commit -m 'auto-sync' ; \
	git push origin gh-pages

gh-pages :
	rm -vfr megaparsec-gh-pages/
	git clone --branch gh-pages $(GH_PAGES) megaparsec-gh-pages

build : index
	stack build
	stack exec site clean
	stack exec site build

index :
	curl -Lo index-original.md $(README)
	tail -n +2 index-original.md > index-without-header.md
	cat index-header.md index-without-header.md > index.md
	rm -v index-original.md index-without-header.md
