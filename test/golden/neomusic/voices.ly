\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    \tempo 4 = 100
    << { c'4 d'4 e'4 f'4 | g'4 a'4 b'4 c''4 |  } \\ { c''2 b'2 | a'2 g'2 |  } >>
  }
  \new Staff { \clef bass \time 4/4 \key c \major
    \tempo 4 = 100
    << { r1 | r1 |  } >>
  }
 >>
 \layout { }
}
