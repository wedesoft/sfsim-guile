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

def average points
  points.inject(:+) * (1.0 / points.size)
end

def vector arg
  arg[0] - arg[1]
end

def gjk a, b
  w = []
  v = [average(a), average(b)]
  while true
    puts "closest #{v}"
    wk = [support_point(a, -vector(v)), support_point(b, +vector(v))]
    puts "candidates #{wk}"
    return v if support(vector(v), -vector(v)) >= support(vector(wk), -vector(v)) - 1e-6
    w += [wk]
    v, w = simplex_closest_point w
  end
end

def simplex_closest_point simplex
  if simplex.size == 1
    return simplex[0], simplex
  else
    p = vector(simplex[0])
    n = simplex[1 ... simplex.size].collect { |s| vector(s) - p }
    t = Linalg.lstsq(NArray[*n.collect(&:to_a)].transpose, -NArray[*p])[0].to_a
    if t.all? { |ti| ti >= 0 } and t.inject(:+) <= 1
      pa = simplex[0][0] + (1 ... simplex.size).zip(t).collect { |i, ti| (simplex[i][0] - simplex[0][0]) * ti }.inject(:+)
      pb = simplex[0][1] + (1 ... simplex.size).zip(t).collect { |i, ti| (simplex[i][1] - simplex[0][1]) * ti }.inject(:+)
      return [pa, pb], simplex
    else
      simplex.subsets.collect do |s|
        simplex_closest_point s
      end.argmin { |p, s| vector(p).norm }
    end
  end
end

a = [Vector[1, 4], Vector[3, 4], Vector[3, 1], Vector[1, 1]]
b = [Vector[-1, 3], Vector[0.9, 2], Vector[-1, 2]]
gjk a, b

a = [Vector[1, 4], Vector[3, 4], Vector[3, 1], Vector[1, 1]]
b = [Vector[-1, 3], Vector[2, 2], Vector[-1, 2]]
gjk a, b

a = [Vector[-1, 0, 1], Vector[1, 0, 1]]
b = [Vector[0, -1, -1], Vector[ 0, 1, -1]]
gjk a, b
