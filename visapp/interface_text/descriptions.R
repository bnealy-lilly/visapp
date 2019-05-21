## Descriptions of animation types, input in app.R

rainfall_desc <- HTML("This animation plots the change in a health measure relative to
                      the baseline value of the measure over time. The animation is designed
                      such that improvement in condition causes the points to fall, causing
                      the plot to look like rain falling. <br><br>
                      
                      This animation is produced from a custom data set derived from ADaM data.
                      The user must select the parameter code, population flags, and other 
                      values displayed on the application.")

spider_desc <- HTML("This animation plots the percent change for three or more 
                    variables of interest in two-dimensional space relative to time. 
                    Each of the selected variables are plotted on their own axis. 
                    At the start of the animation, or baseline (e.g. Visit 1), 
                    all variables are at the maximum percent change (100%) for 
                    each treatment. From baseline to the maximum time point, percent 
                    change from baseline of each variable are plotted by treatments. 
                    In order to make the animation smooth, the data from one time 
                    point to another are interpolated. The spider animation 
                    allows the user to view each variable's percent change from baseline 
                    among the treatments as well as compared to the other variables. <br><br>


                    This animation is produced from a custom data set derived from ADaM data. 
                    The user must select the domain, parameter code, and 
                    analysis flag of interest. The user then selects 
                    a unique column name for the calculated percent 
                    change of each variable. After selecting these variables, 
                    the final dataset will contain the time variable, treatments, 
                    and the corresponding percent differences for the variables of interest. 
                    An example of these parameters is (domain=adeasi, 
                    PARAMCD=EASITS, AN06FL=Y, col_name= easi).")


no_desc <- HTML("<b> Please select a template using the dropdown menu above.</b>")
blank <- HTML("")

# add screenshot for data format