cd /Users/lucaverginer/Research/1_Projects/Code/herding/herding-analysis
import delimited using data/processed/panel_data.csv, clear

use data/processed/panel_data.dta,clear


encode Participant_id, generate(pid)
encode sex, generate(sexl)

xtset pid Round


xtlogit decision round sexl c.timespent##c.round

gen ltime=log(TimeSpent)
gen majority_C = n_cooperators > 10

gen risky = (bret_1_player_boxes_collected-50)/100
gen lexp = log(Total_approvals + Total_rejections)

gen lcoop = log(n_cooperators)



xtlogit Decision majority_C#c.n_cooperators majority_C#c.risky majority_C#c.lexp majority#c.ltime

xtlogit Decision majority_C#c.lcoop majority_C#c.risky majority_C#c.lexp ltime



eststo majC:xtlogit Decision c.n_cooperators c.risky c.lexp c.ltime if majority_C==1 & n_cooperators!=10
eststo majD:xtlogit Decision c.n_cooperators c.risky c.lexp c.ltime if majority_C==0 & n_cooperators!=10


esttab majC majD, wide



xtlogit Decision majority_C#c.n_cooperators majority#c.ltime majority_C#c.risky majority_C#c.lexp if n_cooperators!=10


xtlogit Decision majority_C#c.n_cooperators majority_C#c.risky if n_cooperators!=10


xtlogit Decision majority_C#c.n_cooperators majority#c.ltime majority_C#c.risky if n_cooperators!=10


xtlogit Decision majority_C#c.lcoop majority#c.ltime majority_C#c.risky  majority_C#c.lexp  majority_C#c.Age  if n_cooperators!=10


xtlogit Decision majority_C#i.n_cooperators majority#c.ltime majority_C#c.risky  majority_C#c.lexp  majority_C#c.Age  if n_cooperators!=10
