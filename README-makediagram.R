grViz("
  digraph remarkrexample {

  graph [rankdir = LR]

  node [shape=circle, style=filled, color=gray, fontname = Hack]

  node [fillcolor = beige, shape = rect]
  a b c

  node [fillcolor = steelblue, shape = oval]
  b c d e f

  node [fillcolor = green, shape = square]
  g

  node [fontname = Courier]
  a [label='Data 1']
  b [label='Data 2']
  c [label='Data 3']
  d [label='Package A']
  e [label='Package B']
  f [label='Package C']
  g [label='Report']

  b -> d
  a -> d
  c -> e
  d -> f
  e -> f
  f -> g
  }
", engine = "dot")
