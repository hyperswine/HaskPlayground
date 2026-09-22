\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    \tempo 4 = 120
    << { c'8 d'8 e'8 f'8 g'8 a'8 b'8 c''8 |  } >>
  }
  \new Staff { \clef bass \time 4/4 \key c \major
    \tempo 4 = 120
    << { r1 |  } >>
  }
 >>
 \layout { }
}
