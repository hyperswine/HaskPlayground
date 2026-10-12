\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    \tempo 4 = 84
    << { c'4_\markup \small "0"\glissando e'4_\markup \small "4"\glissando g'4_\markup \small "7"\glissando c''4_\markup \small "12" | a'2^\markup \italic "bend"_\markup \small "9" g'2^\markup \italic "vib."_\markup \small "7"~ | g'2 r2 |  } >>
  }
  \new Staff { \clef bass \time 4/4 \key c \major
    \tempo 4 = 84
    << { r1 | <c g>1_\markup \italic "cresc."_\markup \small \override #'(baseline-skip . 1.7) \center-column { "-5" "-12" } | <c g>1_\markup \italic "cresc."_\markup \small \override #'(baseline-skip . 1.7) \center-column { "-5" "-12" } |  } >>
  }
 >>
 \header { piece = \markup \small "Numbers under the notes are Neo steps: 0 = C4" }
 \layout { }
}
