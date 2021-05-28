#!/usr/bin/env python3

# If you want to browse the Carp documentation offline with all the bells and whistles 
# (e.g. working hyperlinks to other documents and embedded graphics) that are usually 
# provided by the github webpage, you can convert it to static html with this script.
#
# It needs Python3 to work. Install it with:
#     sudo pip install markdown
# or depending on you system configuration (e.g. on Windows):
#     pip install markdown
# 
# Start it with:
#     python3 zConvertMd2Html.py 
# or depending on you system configuration (e.g. on Windows):
#     python zConvertMd2Html.py
# or by executing the script itself (only on *NIX / BSD):
#     chmod u+x zConvertMd2Html.py
#     ./zConvertMd2Html.py
#
# Please note: 
#    inter-document references do not work since the markdown-library does not create 
#    the required html-anchor elements for headings. :-/

import markdown, glob, os, re, shutil

# TODO: copy sub folders and check that docs in ./core/ and ./sdl/ are linked (as html)
# TODO: update Libraries.md to point to local AND web docu source??
# TODO: verify that documents are linked where appropriate
# TODO: update build to include the new folder  

docPath = "../docs/"
docPathLen = len(docPath)
pat = re.compile("\[(.*?)\]\((.*?)\)")	

def change_md2html( match ):
    # unfortunately we can not use filters inside the regular expression like this:
    #    pat = re.compile("\[(.*?)\]\(((?!http).*?)\.md(#.*?)?\)")	
    # ... because that would create false matches for cases like this: 
    #    [desc1](ref1) txt1 (desc2(ref2). txt2 [desc3](https://url1/doc1#anchor1)
    # since the match is still too greedy. 
    mgroups = match.groups() 
    refParts = mgroups[1].split("#")
    refFile  = refParts[0]
    lFile = refFile.lower()
    if len(refParts) < 2:
        refAnchor = ""
    else:
        refAnchor = "#" + refParts[1]
    if lFile.startswith("http") or not lFile.endswith(".md") or lFile.startswith("../"):
        # not a reference to a local .md file => return unchanged match
        mSpan = match.span()
        new = match.string[ mSpan[0]: mSpan[1] ]
        print("\tkept reference: " + new)
    else:
        new = "[%s](%s.html%s)" % (mgroups[0], refFile[:-3], refAnchor )
        print("\tadjusted reference: " + new)
    return new

for fname in glob.glob(docPath+"*"):
    if not os.path.isfile(fname):
        print("ignoring directory: " + fname)
        continue
    elif fname.endswith(".md"):
        print( "converting to html: " + fname)
        # replace any link "[desc](ref.md)" with "[desc](ref.html" if ref does not start with "http" since we only want to change locally available documentation
        with open(fname) as finput:
            content = finput.read()
        new_content = re.sub(pat, change_md2html, content)
        with open( fname[docPathLen:-3] + '.html', 'w') as foutput:
            foutput.write( markdown.markdown( new_content, extensions=['fenced_code', 'codehilite'] ) )
    else:
        # any other files might be resources (e.g. pictures) which probably need to be copied
        print( "copying: " + fname )
        shutil.copy( fname, ".")

print("\ndone :-)")
