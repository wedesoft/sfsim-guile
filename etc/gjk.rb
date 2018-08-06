require 'numo/linalg'
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
  while true
    wk = support_point points, -v
    return v, w if support(v, -v) >= support(wk, -v) - 1e-6
    w += [wk]
    v, w = simplex_closest_point w
  end
end

def simplex_closest_point simplex
  if simplex.size == 1
    return simplex[0], simplex
  else
    p = simplex[0]
    n = simplex[1 ... simplex.size].collect { |s| s - p }
    t = Linalg.lstsq(NArray[*n.collect(&:to_a)].transpose, -NArray[*p])[0].to_a
    if t.all? { |ti| ti >= 0 } and t.inject(:+) <= 1
      return p + n.zip(t).collect { |ni, ti| ni * ti }.inject(:+), simplex
    else
      simplex.subsets.collect do |s|
        simplex_closest_point s
      end.argmin { |p, s| p.norm }
    end
  end
end

a = [Vector[1, 4], Vector[3, 4], Vector[3, 1], Vector[1, 1]]
b = [Vector[-1, 3], Vector[1, 2], Vector[-1, 2]]
d = minkowski_difference a, b
gjk d

simplex_closest_point [Vector[-1, -1], Vector[3, 2]]
simplex = [Vector[-1, -1], Vector[3, 2]]
