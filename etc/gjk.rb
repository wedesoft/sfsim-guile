require 'numo/linalg'
require 'rmagick'
require 'matrix'
include Numo


class Array
  def argmax
    result = self[0]
    each do |item|
      result = item if yield(result) < yield(item)
    end
    result
  end

  def argmin
    result = self[0]
    each do |item|
      result = item if yield(result) > yield(item)
    end
    result
  end

  def subsets
    (0 ... size).collect do |i|
      self[0 ... i] + self[i + 1 ... size]
    end
  end
end

def support point, vector
  point.inner_product vector
end

def support_point points, vector
  points.argmax { |point| support point, vector }
end

def minkowski_difference a, b
  a.collect { |x| b.collect { |y| x - y } }.flatten 1
end

def average points
  points.inject(:+) * (1.0 / points.size)
end

def gjk points
  w = []
  v = average points
  t = [1]
  while true
    wk = support_point points, -v
    return v, w, t if support(v, -v) >= support(wk, -v) - 1e-6
    w += [wk]
    v, w, t = simplex_closest_point w
  end
end

def simplex_closest_point simplex
  if simplex.size == 1
    return simplex[0], simplex, [1]
  else
    p = simplex[0]
    n = simplex[1 ... simplex.size].collect { |s| s - p }
    t = Linalg.lstsq(NArray[*n.collect(&:to_a)].transpose, -NArray[*p])[0].to_a
    if t.all? { |ti| ti >= 0 } and t.inject(:+) <= 1
      return p + n.zip(t).collect { |ni, ti| ni * ti }.inject(:+), simplex, t
    else
      simplex.subsets.collect do |s|
        simplex_closest_point s
      end.argmin { |p, s, t| p.norm }
    end
  end
end

def lookup a, b, simplex
  simplex.collect do |s|
    result = nil
    for x in a
      for y in b
        result = [x, y] if x - y == s
      end
    end
    result
  end
end

a = [Vector[1, 4], Vector[3, 4], Vector[3, 1], Vector[1, 1]]
#b = [Vector[-1, 3], Vector[0.9, 2], Vector[-1, 2]]
b = [Vector[-1, 3], Vector[-1, 2], Vector[0.9, 2]]
d = minkowski_difference a, b
c = gjk d
c[0] # vector
c[1] # simplex
c[2] # linear-combination
lookup(a, b, c[1])
# linear-combine collision point
# t und x oder t und y -> Kollisionspunkt
# plane-fit SVD von Vektoren vom Punkt aus -> Normale
# oder: epsilon Abstand ergibt Normale
# [0.9, 2]
# [1, 2]
