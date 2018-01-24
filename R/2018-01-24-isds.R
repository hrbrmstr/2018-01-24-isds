#' ---
#' title: "ISDS Webinar"
#' author: "Bob Rudis"
#' date: "2018-01-24"
#' output:
#'   html_document:
#'     keep_md: true
#'     highlight: pygments
#' ---

# This is a plain R script you can use as-is, but it's also "spinnable".
# If you use RStudio, go and try to knit it. It will work just like Rmds.

#+ init, include=FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE, dev="png",
                      fig.retina = 2, fig.width = 10, fig.height = 6)
#+ libs
# for stat_xspline
library(ggalt) # devtools::install_github("hrbrmstr/statebins")

# for animation
library(magick)

# why you came!
library(cdcfluview)

# package that uses themes you saw in the presentation
library(hrbrthemes)

# makes those block state choropleth-y cartograms
library(statebins)

# all the goodness from Hadley, RStudio, et al
library(tidyverse)

#' ## ILI Basic examples

#+ eval=TRUE
ili <- ilinet("national", years = 2013:2018)

glimpse(ili)

hhs <- ilinet("hhs", years = 2013:2018)

glimpse(hhs)

update_geom_font_defaults(font_rc)
theme_set(theme_ipsum_rc(grid="XY", strip_text_face = "bold"))

#+ national_ili
select(ili, week_start, starts_with("age")) %>%
  select(-age_25_64) %>%
  gather(group, ct, -week_start) %>%
  mutate(group = factor(group, levels=c("age_0_4", "age_5_24", "age_25_49", "age_50_64", "age_65"),
                        labels=c("Ages 0-4", "Ages 5-24", "Ages 25-49", "Ages 50-64", "Age 65+"))) %>%
  ggplot(aes(week_start, ct, group=group)) +
  stat_xspline(geom="area", aes(color=group, fill=group), size=2/5, alpha=2/3) +
  scale_x_date(expand=c(0,0), date_labels="%b\n`%y") +
  scale_y_comma() +
  scale_color_ipsum() +
  scale_fill_ipsum() +
  labs(x=NULL, y="(weekly)\n# Patients Reported",
       title="Weekly reported Influenza-Like Illness (ILI) — U.S./National by Age Group",
       subtitle="All age groups except 5-24 are reporting larger number of cases this season than the previous four seasons",
       caption="Flu Seasons 2013-14 / 2014-15 / 2015-16 / 2016-17 / 2017-18\nSource: CDC/#rstats cdcfluview"
  ) +
  facet_wrap(~group, scales="free_x", nrow=2) +
  theme(axis.text.x=element_text(size=9)) +
  theme(legend.position="none")

#+ hhs_region_ili
ggplot(hhs, aes(week_start, weighted_ili, group=region)) +
  stat_xspline(geom="area", aes(color=region, fill=region), size=2/5, alpha=2/3) +
  geom_smooth(se=FALSE, size=1, color="#2b2b2b") +
  scale_x_date(expand=c(0,0), date_labels="%b\n`%y") +
  scale_y_comma() +
  ggthemes::scale_color_tableau() +
  ggthemes::scale_fill_tableau() +
  labs(x=NULL, title="Weighted ILI by HHS Region (2013/4-2017/8)", caption="Source: CDC/#rstats cdcfluview") +
  facet_wrap(~region, scales="free_x", nrow=2) +
  theme(axis.text.x=element_text(size=9)) +
  theme(legend.position="none")

#' ## The "statebins" examples

#+ statebins_ili
flu <- ili_weekly_activity_indicators(2017)

filter(flu, weekend == last(weekend)) %>%
  statebins(state_col = "statename", value_col = "activity_level", round = TRUE,
            ggplot2_scale_function = viridis::scale_fill_viridis,
            name = "ILI Activity Level  ") +
  labs(title=sprintf("U.S. ILI Weekly Activity : Week Ending %s / 2017-18 Season", last(flu$weekend))) +
  theme_statebins(base_family="Roboto Condensed")

#' ## Mortality example

#+ mortality
mort <- pi_mortality(years=2013:2018)

select(mort, wk_end, baseline, threshold, percent_pni) %>%
  gather(measure, value, -wk_end) %>%
  arrange(wk_end) %>%
  ggplot(aes(wk_end, value, group=measure)) +
  geom_line(aes(color=measure), size=1) +
  labs(
    x=NULL, y="% of ALL Deaths Due to P&I",
    title="Percentage of all deaths due to pneumonia and influenza, National Summary",
    subtitle="2013-2018"
  ) +
  theme_ipsum_rc(grid="XY", base_size=16, plot_title_size=24) +
  scale_color_ipsum(name=NULL) +
  scale_y_percent() +
  theme(legend.position="bottom") +
  theme(legend.direction="horizontal")

#' ## Animation
#'
#' The following makes the animation, but can take a bit, so it's off by default.

#+ animation, eval=FALSE
flu <- ili_weekly_activity_indicators(2017)

frames <- image_graph(width=1800, height=1200, res=144)

arrange(flu, weekend) %>%
  pull(weekend) %>%
  unique() %>%
  map(~{
    filter(flu, weekend == .x) %>%
      statebins(state_col = "statename", value_col = "activity_level", round = TRUE,
                ggplot2_scale_function = viridis::scale_fill_viridis, limits=c(0,10),
                name = "ILI Activity Level  ") +
      labs(title=sprintf("U.S. ILI Weekly Activity : Week Ending %s / 2017-18", .x)) +
      theme_statebins(base_family="Roboto Condensed") -> gg
    print(gg)
  }) -> y

gif <- image_animate(frames, 1)
image_write(gif, "fluview.gif")
