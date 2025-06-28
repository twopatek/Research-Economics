br>
  <br>
  <br>
  <br>
  The final section of this report focuses on the revisions themselves and what story they tell. The analysis considers two revision variances: Third Estimate minus Second Estimate and Third Estimate minus Advance Estimate. 
<br>
  <br>
  Comparing Third Estimate to Second Estimate focuses on the changes made between the second and third estimates, providing insights into the recent adjustments to the data.It helps assess the stability of the estimates and whether there have been material revisions in the latest release. This analysis is more targeted at understanding how the most recent data releases have influenced the overall picture.
<br>
  <br>
  Comparing Third Estimate to Advance Estimate helps you understand how much the initial estimate (advance estimate) has changed as more data becomes available.
It provides insights into the revisions made to the economic data over the estimation period. This analysis helps assess the accuracy of the initial estimate and whether there were significant adjustments based on additional information.

The analysis below creates two data sets.
```{r revisions}
revisions_by_name <- df %>% 
  filter(estimate == "third",
         release_type == "delta") %>% 
  select(-estimate_var) %>% 
  group_by(name) %>% 
  mutate(rev3_mean = round(mean(full_estimate_var), 2)) %>% 
  mutate(sd = sd(value))



```

```{r }
revisions_q <- df %>% 
  filter(estimate == "third",
         release_type == "delta") %>% 
  group_by(quarter) %>% 
  summarise(rev2_mean = round(mean(estimate_var), 2), rev3_mean = round(mean(full_estimate_var), 2))

revisions_q

revisions_q
```

