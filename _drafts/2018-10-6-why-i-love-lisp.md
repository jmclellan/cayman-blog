# Why I love lisp

It seems like a common experience for lispers to have to defend their language of choice. Maby people always feel the need to differentiate their language from others, to prove that the thing they like is the same thing that other people should like. Or maby I'm actually trying to convert my friends and prove that the 60 year old language still has some clout in the technology world today. Regardless of the reason I do often tell people that I love lisp and they often crank their necks and give me looks of confusion. 

## what is lisp?
Lisp is one of the oldest high level languages still in use today. 



### what is great about it



mostly functional style 

meta-programming (macros)

multi-paradigm

# what is a lisp macro
Here is the thing about programming, its all about abstraction. we take ideas, we isolate them, define them, and then reuse them along with other ideas to solve bigger problems. Its all about patterns. I have a subtask that i find multiple times, i know that i can turn that into a function and reuse that. This is at the core of what programming is in my mind. Macros take abstraction and apply them to programming. 

A nice little example is going to be python. There are plenty of times where I want to open up a file and write something to it. After I write something to this file, as a responsible programmer I want to close out the connection to that file. This i would say constitutes a pretty common pattern in my code.

```python
file = open(“testfile.txt”, “w”)

file.write(“This is a test”) 
file.write(“To add more lines.”)

file.close()
```

the 1 to 1 lisp code would be 
```lisp
(defvar file (open "testfile.txt"))

(print "this is a test" file)
(print "to add more lines." file)

(close file)
```
The lisp macros allow us to write code that actually generates more code, the generated code is then evaluated/compiled to have the same effect. 



