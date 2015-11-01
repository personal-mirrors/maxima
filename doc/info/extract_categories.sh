TARGET=$1
if [ "x$TARGET" = "x" ]; then
  echo USAGE: sh $0 '<TARGET>'
  exit 1
fi

set -x

TARGET_TEXI=$TARGET.texi

WORKING_DIRECTORY=`mktemp -d ${TMPDIR:-/tmp}/maxima-texinfo-categories-XXXXXX`
cp -R *.texi figures $WORKING_DIRECTORY
d=`pwd`
cd $WORKING_DIRECTORY

for f in *.texi; do
  if [ $f = "maxima.texi" ]
    then echo SKIP OVER $f
    else
      sed 's/^@deffn  *{\([^}]*\)}  *\([^[:blank:]]*\).*/@anchor{Item: '$f'_fn_\2_\1}\
&/; s/^@defvr  *{\([^}]*\)}  *\([^[:blank:]]*\).*/@anchor{Item: '$f'_vr_\2_\1}\
&/; s/^@node  *\([^,]*\).*/@anchor{Item: '$f'_nd_\1}\
&/' "$f" > tmp.texi
      mv tmp.texi "$f"
    fi
done

rm -f tmp-make-categories.py
for i in *.texi; do
  cat $i \
  | awk '!/^@c / && !/^@c$/ && (/^@deffn/ || /^@defvr/ || /^@end deffn/ || /^@end defvr/ || /@category/ || /@node/)'\
  | sed 's/\$/---endofline---/'\
  | sed -f "$d/extract_categories1.sed" \
  | sed -e 's,"fn_,"'$i'_fn_,g' -e 's,"vr_,"'$i'_vr_,g' -e 's,"nd_,"'$i'_nd_,g';
done | awk -F'$' -f "$d/extract_categories1.awk" \
  | sed 's/---endofline---/$/'\
  >> tmp-make-categories.py

${PYTHONBIN:-python} tmp-make-categories.py

sed 's/^@bye//' $TARGET_TEXI > tmp-target.texi
echo '@node Documentation Categories' >> tmp-target.texi
echo '@chapter Documentation Categories' >> tmp-target.texi
for f in Category-*.texi; do echo '@include' $f; done >> tmp-target.texi
echo '@bye' >> tmp-target.texi
mv tmp-target.texi $TARGET_TEXI

cat include-maxima.texi \
    | awk '/^@comment @detailmenu/ {printf("Documentation Categories\n* Documentation Categories::\tDocumentation categories.\n\n");} {print}' \
    >tmp && mv tmp include-maxima.texi

# perl "/usr/bin/texi2html" -split_chapter --lang=en --output=. \
#  --css-include="$d/manual.css" --init-file "$d/texi2html.init" $TARGET_TEXI
makeinfo --no-warn --html --output=. --css-include="$d/manual.css" --split=chapter --init-file="$d/texi2html.init" $TARGET_TEXI 

# Now clean up the texi2html output. I'm going to burn in Hell for this (and much else).

for f in *.html
do
    grep -q '<a href=".*">Category: .*</a>' $f
    if [ $? = 0 ]; then
        echo FIX UP CATEGORY BOXES IN $f
        sed 's/^&middot;$//; s/<p>\(<a href=".*">Category: .*<\/a>\)/<p>Categories:\&nbsp;\&nbsp;\1/' $f > tmp.html
        mv tmp.html $f
    fi
done

for f in *.html
do
    grep -q '<a href=".*">Category: .*</a>' $f
    if [ $? = 0 ]; then
        echo FIX UP CATEGORY HREFS IN $f
        sed 's/<a href="\(.*\)">Category: \(.*\)<\/a>/<a href="\1">\2<\/a>/' $f > tmp.html
        mv tmp.html $f
    fi
done

for f in *.html
do
    grep -q '<a href=".*">Item: .*</a>' $f
    if [ $? = 0 ]; then
        echo FIX UP ITEM HREFS IN $f
        sed 's/<a href="\(.*\)">Item: \(.*\)<\/a>/<a href="\1">\2<\/a>/' $f > tmp.html
        mv tmp.html $f
    fi
done

mv *.html "$d"

cd "$d"
set +x
