from functools import reduce
class TreeNode:
	def __init__(self, value=None, children=None):
		self.value = value
		if children:
			self.children = children
		else:
			self.children = []

	def __str__(self):
		return str(self.value)

def num_nodes(root):
	return sum(map(num_nodes, root.children)) + 1


def contains(root, value):
	return root.value == value or any(contains(r, value) for r in root.children)


def preorder(root):
	'''
	reduce is foldl lol
	'''
	return [root.value] + reduce(lambda rec_return, x: rec_return + preorder(x), root.children, [])




if __name__ == "__main__":
	root = TreeNode (1, 
		[TreeNode(2, [TreeNode(5), TreeNode(6)]),
		TreeNode(3, [TreeNode(7), TreeNode(8), TreeNode(9)]),
		TreeNode(4)])

	print(num_nodes(root))
	print(contains(root, 7))
	print(contains(root, 90))
	print(preorder(root))
