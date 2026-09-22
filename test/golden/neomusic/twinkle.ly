\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    \tempo 4 = 100
    << { c'4 c'4 g'4 g'4 | a'4 a'4 g'2 | f'4 f'4 e'4 e'4 | d'4 d'4 c'2 | g'4 g'4 f'4 f'4 | e'4 e'4 d'2 | g'4 g'4 f'4 f'4 | e'4 e'4 d'2 | c'4 c'4 g'4 g'4 | a'4 a'4 g'2 | f'4 f'4 e'4 e'4 | d'4 d'4 c'2 |  } >>
  }
  \new Staff { \clef bass \time 4/4 \key c \major
    \tempo 4 = 100
    << { <c g>1 | <f a>2 <c g>2 | <f a>2 <c g>2 | <g, d>2 <c g>2 | <c g>2 <f a>2 | <c g>2 <g, d>2 | <c g>2 <f a>2 | <c g>2 <g, d>2 | <c g>1 | <f a>2 <c g>2 | <f a>2 <c g>2 | <g, d>2 <c g>2 |  } >>
  }
 >>
 \layout { }
}
