class Node:
    def __init__(self, text, index, depth):
        self.text_content = text
        self.index = index
        self.depth = depth
        self.children = []
        self.parents = []

    def add_parent(self, all_nodes, parent):
        parent_i = lookup_course(all_nodes, parent)
        if parent_i > -1:
            if all_nodes[parent_i] in self.parents: #Add only if not already in parents
                print("Course is already in parents: "+all_nodes[parent_i].number)
            else:
                self.parents.append(courses[parent_i])

    def add_child(self, child):
        if not child in self.children:
            self.children.append(child)

    def add_to_parents(self):
        for parent in self.parents:
            parent.add_follow_up(self)

def parse_AST():
    # Special-case for 1st node
    current_node = Node("",0,1)
    nodes = [current_node]
    depth = 0
    start_count = 0
    end_count = 0

    string = read_AST()
    for c in string:
        if c == "(" or c == "[":
            start_count += 1
            depth += 1
            if start_count > 1:
                new_node = Node("",start_count-1, depth)
                current_node.children.append(new_node)
                new_node.parents.append(current_node)
                current_node = new_node
                nodes.append(current_node)

        elif c == ")" or c == "]":
            end_count += 1
            depth -= 1
            #print(current_node.text_content)
            if len(current_node.parents) == 1:
                current_node = current_node.parents[0]
            elif len(current_node.parents) > 1:
                raise ZeroDivisionError('more than 1 parent! Shouldn\'t happen!')
        elif start_count > 0:
            current_node.text_content += c
    if not start_count == end_count:
        raise ZeroDivisionError('more of one kind of parenthesiseses than the other!')
    return nodes


def read_AST():
    nodes = []
    fname = "ptree.txt"
    counter = 0
    with open(fname) as f:
        lines = f.readlines()
        for line in lines:
            # print(len(line))
            line = line.strip('\n')
            line = line.replace('"',"'")
            return line
