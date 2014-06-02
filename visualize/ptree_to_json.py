from div_classes import *

nodes = parse_AST()


print("{")
print(" \"nodes\":[")

i = 0
for node in nodes:
    if i != 0:
        print(",")
    print(" {\"number\":\""+node.text_content+"\",\"group\":"+str(node.depth)+"}"),
    i +=1

print(" ")
print(" ],")
print(" \"links\":[")

first_found = 0
for node in nodes:
    for parent in node.parents:
        if first_found != 0:
            print(",")
        print(" {\"source\":"+str(node.index)+", \"target\":"+str(parent.index)+", \"value\":1}"),
        first_found += 1

print(" ")
print(" ]")
print("}")
