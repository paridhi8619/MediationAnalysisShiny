# MediationAnalysisShiny

This application has been developed to demonstrate the mediation analysis and data visualization using RShiny.

The application's UI layout has four tabs:

1. Data (Data read and showed)
2. About (This tabs talks about the data)
3. Mediation Analysis (This tab shows the plot depicting the treatment effect. The mediation analysis charts and tables
			can be seen for variables 'itch', 'BSA' and 'redness' by selecting one of them using the dropdown list)
4. Simulation (In this tab, user specifies a seed adn the number of observations to be generated for each treatment viz. Placebo and Rx
		user can specify the value of mediation that they want for mediating variable 'itch'. This value (between 0 and 1)
		is used in data generation for DLQI and it will control the proportion of DLQI that is taken from 'itch'. The output qualtity
		'proportion mediated' should be directly proportional to the slider position.
		Once specified, data is simulated and the mediation analysis is performed for all three mediating variables
		'itch', 'BSA' and 'redness'.)
		From this tab, user will be able to download the simulated data as well as the one with the randomized missing values
		using the buttons provided on this page. User can select a location on their desktop if the application is running
		internally or the file will be saved to their default download location if the application is running in the browser)

-------------------------------------------
Some of the observations about the results:|
-------------------------------------------

1. Plot on mediation analysis tab shows, there is an effect of 'TRT' on 'DLQI' (dermatology life quality index).
2. Mediation plot shows that:
	- ACME ( Avg. Causal Mediated Effect) of 'itch' on 'DLQI' is -3.867, and it has a significant mediating effect, which can be
		confirmed with very low p value.	
	- ACME for BSA and redness are found to be non-significant. So their mediation can not be proved using this analysis.
3. The chart showing the mediation effect displays the regression coefiicients and corresponding p values for each of the interations.
   The indirect effect from TRT to DLQI = -1.437 * 2.66 ~ -3.87, which is ACME for itch.
4. On simulation tab, as the mediation effect is varied, the effect on the mediation plot of itch can be seen clearly, the ACME
   increases with increased mediation, so it the 'proportion mediated' shown on top of the charts.
5. Changing the sample size makes rejecting the null hypothesis (that the treatment has no effect on the DLQI) easy for the same margin.
6. The sensitivity plot shows the mediation models is quite robust as rho required for the ACME to switch the signs is > 0.

#############################################################################################################################################
References:
1. Baron, R., & Kenny, D. (1986). The moderator-mediator variable distinction in social psychological research: Conceptual, strategic,
   and statistical considerations. Journal of Personality and Social Psychology, 51, 1173-1182.
2.Zhang, Zhongheng, Cheng Zheng, Chanmin Kim, Sven Van Poucke, Su Lin, and Peng Lan. "Causal mediation analysis in the context 
  of clinical research." Annals of translational medicine 4, no. 21 (2016). 
3. Tingley, D., Yamamoto, T., Hirose, K., Imai, K. and Keele, L. (2014). "mediation: R package for Causal Mediation Analysis", Journal
   of Statistical Software, Vol. 59, No. 5, pp. 1-38.
