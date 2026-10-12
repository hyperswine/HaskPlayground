\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    \tempo 4 = 84
    << { c'4\glissando e'4\glissando g'4\glissando c''4 | a'2^\markup \italic "bend" g'2^\markup \italic "vib."~ | g'2 r2 |  } >>
  }
  \new Staff { \clef bass \time 4/4 \key c \major
    \tempo 4 = 84
    << { r1 | <c g>1_\markup \italic "cresc." | <c g>1_\markup \italic "cresc." |  } >>
  }
 >>
 \layout { }
}
