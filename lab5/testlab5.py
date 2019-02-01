"""Tester for CSCC24 Lab 5"""

import lab5
import unittest

class TestRecursion(unittest.TestCase):

    m1 = [[1, 0, 2],
          [2, 1, 4],
          [-1, 1, -1]]
    m2 = [[1, 2, 3],
          [4, 5, 6],
          [7, 8, 9]]
    m3 = [[2, 2, 5],
          [6, 6, 10],
          [6, 9, 8]]
    m4 = [[15, 18, 21],
          [34, 41, 48],
          [-4, -5, -6]]
    m5 = [[2, 0, 4],
          [4, 2, 8],
          [-2, 2, -2]]
    m1tr = [[1, 2, -1],
            [0, 1, 1],
            [2, 4, -1]]

    def testDotProduct1(self):
    
        self.assertEqual(lab5.dot_product1([], []), 0)
        self.assertEqual(lab5.dot_product1([1], [-2]), -2)
        self.assertEqual(lab5.dot_product1([1, -2, 6], [-2, 0, 1]), 4)

    def testDotProduct2(self):
    
        self.assertEqual(lab5.dot_product2([], []), 0)
        self.assertEqual(lab5.dot_product2([1], [-2]), -2)
        self.assertEqual(lab5.dot_product2([1, -2, 6], [-2, 0, 1]), 4)

    def testDotProduct3(self):
    
        self.assertEqual(lab5.dot_product3([], []), 0)
        self.assertEqual(lab5.dot_product3([1], [-2]), -2)
        self.assertEqual(lab5.dot_product3([1, -2, 6], [-2, 0, 1]), 4)

    def testVectorAdd1(self):
    
        self.assertEqual(lab5.vector_add1([], []), [])
        self.assertEqual(lab5.vector_add1([1], [-2]), [-1])
        self.assertEqual(lab5.vector_add1([1, -2, 6], [-2, 0, 1]), [-1, -2, 7])    

    def testVectorAdd2(self):
    
        self.assertEqual(lab5.vector_add2([], []), [])
        self.assertEqual(lab5.vector_add2([1], [-2]), [-1])
        self.assertEqual(lab5.vector_add2([1, -2, 6], [-2, 0, 1]), [-1, -2, 7])   

    def testadd1(self):
        self.assertEqual(lab5.add1(self.m1, self.m2), self.m3)

    def testadd2(self):
        self.assertEqual(lab5.add2(self.m1, self.m2), self.m3)


    def testscalar_vector_mult1(self):
        self.assertEqual(lab5.scalar_vector_mult1(2, [1, 3, 4, 5]), [2, 6, 8, 10])


    def testscalar_vector_mult2(self):
        self.assertEqual(lab5.scalar_vector_mult2(2, [1, 3, 4, 5]), [2, 6, 8, 10])


    def testscalar_mult1(self):
        self.assertEqual(lab5.scalar_mult1(2, self.m1), self.m5)

    def testscalar_mult2(self):
        self.assertEqual(lab5.scalar_mult2(2, self.m1), self.m5)

if __name__ == '__main__':

    unittest.main()
