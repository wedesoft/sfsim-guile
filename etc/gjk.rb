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
  elsif simplex.size == 2
    p = simplex[0]
    n = simplex[1] - p
    t = Linalg.lstsq(NArray[n.to_a].transpose, -NArray[*p])[0][0]
    if t >= 0 and t <= 1
      return p + n * t, simplex
    else
      simplex.subsets.collect do |s|
        simplex_closest_point s
      end.argmin { |p, s| p.norm }
    end
  elsif simplex.size == 3
    p = simplex[0]
    n1 = simplex[1] - p
    n2 = simplex[2] - p
    t = Linalg.lstsq(NArray[n1.to_a, n2.to_a].transpose, -NArray[*p])[0].to_a
    if t[0] >= 0 and t[1] >= 0 and t[0] + t[1] <= 1
      return p + n1 * t[0] + n2 * t[1], simplex
    else
      simplex.subsets.collect do |s|
        simplex_closest_point s
      end.argmin { |p, s| p.norm }
    end
  elsif simplex.size == 4
    p = simplex[0]
    n1 = simplex[1] - p
    n2 = simplex[2] - p
    n3 = simplex[3] - p
    t = Linalg.lstsq(NArray[n1.to_a, n2.to_a, n3.to_a].transpose, -NArray[*p])[0].to_a
    if t[0] >= 0 and t[1] >= 0 and t[2] >= 0 and t[0] + t[1] + t[2] <= 1
      return p + n1 * t[0] + n2 * t[1] + n3 * t[2], simplex
    else
      simplex.subsets.collect do |s|
        simplex_closest_point s
      end.argmin { |p, s| p.norm }
    end
  end
end

gjk [Vector[1, 1], Vector[3, 1], Vector[2, -2], Vector[1, 5]]
