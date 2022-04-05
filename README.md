emoji
------

Single OCaml file containing byte sequences of all the Unicode emoji 
characters and sequences sourced from [here](https://unicode.org/Public/emoji/14.0/emoji-test.txt)
You can print them: 
```
print_endline Emoji.troll
```
🧌

You can get all emojis from the same category or subcategory:
```
let best_animals = Emoji.sub_category_animal_reptile in
Array.iter print_string best_animals;
```
🐉🐊🦎🦖🦕🐢🐲🐍

Emoji include emojis with skin tones:
```
print_endline Emoji.raised_fist_dark_skin_tone
```
✊🏿

Using `ocp-browser` shows the emoji

![ocp-browser screenshot](./ocp-browser-emoji.png)

# Development 

build with
```
$ dune build @all
```
this will generate `emoji.ml` from `./src/gencode.ml`

You can use `./test/test.ml` to test the code.
Which for me resulted in:
![test.ml output](./emojis.png)
