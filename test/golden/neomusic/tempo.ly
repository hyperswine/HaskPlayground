\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    << { c'4_\markup \small "0" d'4_\markup \small "2" e'4_\markup \small "4" f'4_\markup \small "5" | g'4_\markup \small "7" f'4_\markup \small "5" e'4_\markup \small "4" d'4_\markup \small "2" | c'4_\markup \small "0" d'4_\markup \small "2" e'4_\markup \small "4" f'4_\markup \small "5" | g'4_\markup \small "7" f'4_\markup \small "5" e'4_\markup \small "4" d'4_\markup \small "2" | c'8_\markup \small "0" d'8_\markup \small "2" e'8_\markup \small "4" f'8_\markup \small "5" g'8_\markup \small "7" f'8_\markup \small "5" e'8_\markup \small "4" d'8_\markup \small "2" | c'8_\markup \small "0" d'8_\markup \small "2" e'8_\markup \small "4" f'8_\markup \small "5" g'8_\markup \small "7" f'8_\markup \small "5" e'8_\markup \small "4" d'8_\markup \small "2" | c'2\fermata_\markup \small "0" r2 |  } >>
  }
  \new Dynamics { \tempo 4 = 96 s4*8 \tempo \markup \italic "rit."  s4*8 \tempo \markup \italic "a tempo, Swing" 4 = 96 s4*8 \tempo \markup \italic "Straight" 4 = 96 s4*4 }
  \new Staff { \clef bass \time 4/4 \key c \major
    << { <c g>1_\markup \small \override #'(baseline-skip . 1.7) \center-column { "-5" "-12" } | <g, d>1_\markup \small \override #'(baseline-skip . 1.7) \center-column { "-10" "-17" } | <c g>1_\markup \small \override #'(baseline-skip . 1.7) \center-column { "-5" "-12" } | <g, d>1_\markup \small \override #'(baseline-skip . 1.7) \center-column { "-10" "-17" } | <c g>1_\markup \small \override #'(baseline-skip . 1.7) \center-column { "-5" "-12" } | <g, d>1_\markup \small \override #'(baseline-skip . 1.7) \center-column { "-10" "-17" } | <c g>2\fermata_\markup \small \override #'(baseline-skip . 1.7) \center-column { "-5" "-12" } r2 |  } >>
  }
 >>
 \header { piece = \markup \small "Numbers under the notes are Neo steps: 0 = C4" }
 \layout { }
}
