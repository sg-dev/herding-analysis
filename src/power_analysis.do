# GOOD

power twoproportions .5, alpha(.01) diff(0.05(0.005).2) power(.95) graph

power twoproportions .01(0.01).99, alpha(.01) diff(0.095) power(.95) graph


power twoproportions .01(0.01).99, alpha(.01) diff(0.05) power(.95) graph
graph export "~/Downloads/required_size_05.pdf", replace



power twoproportions .01(0.01).99, alpha(.01) diff(0.095) power(.95) graph
graph export "~/Downloads/required_size_095.pdf", replace