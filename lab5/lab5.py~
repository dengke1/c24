'''
Learning objective:
study Python's functional features

We go back to our previous lab, where we practised using higher
order procedures in Scheme. We will re-implement some of the functions
from that lab in Python3.  Once again, we will use lists and lists of
lists to represent vectors and matrices.

Now that you are familiar with the style, albeit in a different
language, it is up to you to find suitable higher order functions or
other funcitonal constructs to use in your solutions.

You should begin by looking at what's available. For example:
  -- generator expressions
  -- list comprehensions
  -- map, reduce, filter, zip, any, all, ...

You will probably need to use zip for most of your solutions.

Finally, as always, make sure your code passes pep8 and pyint.

'''

import functools


def dot_product1(vector1, vector2):
    '''Return dot product of vector1 and vector2. Use a generator.

    '''
    return sum(vector1[i] * vector2[i] for i in range(len(vector1)))


def dot_product2(vector1, vector2):
    '''Return dot product of vector1 and vector2. Use map and lambda.

    '''

    return sum(list(map(lambda x, y: x * y, vector1, vector2)))


def dot_product3(vector1, vector2):
    '''Return dot product of vector1 and vector2. Use reduce and lambda.

    '''

    return functools.reduce(lambda x, t: x + t[0]*t[1], zip(vector1, vector2), 0)


def vector_add1(vector1, vector2):
    '''Return the sum of vector1 and vector2. Use list comprehension.

    '''

    return [vector1[i] + vector2[i] for i in range(len(vector1))]


def vector_add2(vector1, vector2):
    '''Return the sum of vector1 and vector2. Use map and lambda.

    '''

    return list(map(lambda x, y: x + y, vector1, vector2))


def add1(matrix1, matrix2):
    '''Return the sum of matrix1 and matrix2. Use list comprehension.

    '''

    return [vector_add1(matrix1[i], matrix2[i]) for i in range(len(matrix1))]


def add2(matrix1, matrix2):
    '''Return the sum of matrix1 and matrix2. Use map.

    '''

    return list(map(vector_add2, matrix1, matrix2))


def scalar_vector_mult1(k, vector):
    '''Return scalar product of number k and vector.

    '''

    return [k * vector[i] for i in range(len(vector))]


def scalar_vector_mult2(k, vector):
    '''Return scalar product of number k and vector. Give a solution
    different from the one above.

    '''

    return list(map(lambda x: k * x, vector))


def scalar_mult1(k, matrix):
    '''Return scalar product of number k and matrix.

    '''

    return [scalar_vector_mult1(k, matrix[i]) for i in range(len(matrix))]


def scalar_mult2(k, matrix):
    '''Return scalar product of number k and matrix.  Give a solution
    different from the one above.

    '''

    return list(map(scalar_vector_mult2, [k for i in matrix], matrix))
