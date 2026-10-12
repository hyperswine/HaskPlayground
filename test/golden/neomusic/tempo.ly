\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    << { c'4 d'4 e'4 f'4 | g'4 f'4 e'4 d'4 | c'4 d'4 e'4 f'4 | g'4 f'4 e'4 d'4 | c'8 d'8 e'8 f'8 g'8 f'8 e'8 d'8 | c'8 d'8 e'8 f'8 g'8 f'8 e'8 d'8 | c'2\fermata r2 |  } >>
  }
  \new Dynamics { \tempo 4 = 96 s4*8 \tempo \markup \italic "rit."  s4*8 \tempo \markup \italic "a tempo, Swing" 4 = 96 s4*8 \tempo \markup \italic "Straight" 4 = 96 s4*4 }
  \new Staff { \clef bass \time 4/4 \key c \major
    << { <c g>1 | <g, d>1 | <c g>1 | <g, d>1 | <c g>1 | <g, d>1 | <c g>2\fermata r2 |  } >>
  }
 >>
 \layout { }
}
