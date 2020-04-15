import os
import string

prefix = '\\'

for i in range(33, 128):
        if i!=92:                
                with open('rendered_letters/alphabet.txt', 'a') as f:
                        f.write( prefix + chr(i) + "\n" )
                os.system("./render " + str(i) + " >> rendered_letters/alphabet.txt")
