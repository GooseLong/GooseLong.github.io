import json
import sys

"""
chapter1 = json.loads(r'''{
  "passages": [
    {
      "text": "hi my name's chippy\n@@@\n\n[[hi]]\n\n[[what?!]]\n\n[[i don't care]]",
      "links": [
        {
          "name": "hi",
          "link": "hi",
          "pid": "2"
        },
        {
          "name": "what?!",
          "link": "what?!",
          "pid": "3"
        },
        {
          "name": "i don't care",
          "link": "i don't care",
          "pid": "4"
        }
      ],
      "name": "Intro",
      "pid": "1",
      "position": {
        "x": "1180",
        "y": "540"
      }
    },
    {
      "text": "Double-click this passage to edit it.",
      "name": "hi",
      "pid": "2",
      "position": {
        "x": "1133",
        "y": "788"
      }
    },
    {
      "text": "Double-click this passage to edit it.",
      "name": "what?!",
      "pid": "3",
      "position": {
        "x": "1377",
        "y": "687"
      }
    },
    {
      "text": "Double-click this passage to edit it.",
      "name": "i don't care",
      "pid": "4",
      "position": {
        "x": "1520",
        "y": "455"
      }
    }
  ],
  "name": "chippy",
  "startnode": "1",
  "creator": "Twine",
  "creator-version": "2.3.8",
  "ifid": "BAADDCA9-73BC-482E-98D7-4D09F7E0DD5E"
}''')
"""

chapters = []

for arg in sys.argv[1:]:
    #print(open(arg,'r').read())
    chapters.append(json.loads(open(arg,'r').read()))

output = {"story":[]}

def linksToOptions(passage,ic):
    options = {}
    if not ("links" in passage):
        return options
    for link in passage["links"]:
        options[link["name"].lower().replace("\n","<br>")] = ic*100+int(link["pid"])
    return options

for ic,chapter in enumerate(chapters):
    character = chapter["name"]
    for ip, passage in enumerate(chapter["passages"]):
        #print("links" in passage)
        if chapter["name"] == "group":
            character = passage["tags"][0]
        output["story"].append({
            "id":ic*100+int(passage["pid"]),
            "character":character,
            "paragraph":passage["text"].split("\n@@@")[0].lower().replace("\n","<br>"),
            "options":linksToOptions(passage,ic)
        })

print(json.dumps(output,sort_keys=True, indent=4))

