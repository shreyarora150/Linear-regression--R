Y = c(88,80,96,76,80,73,58,116,104,99,64,126,94,71,111,109,100,127,99,82,67,109,78,115,83)
X1 = c(86,62,110,101,100,78,120,105,112,120,87,133,140,84,106,109,104,150,98,120,74,96,104,94,91)
X2 = c(110,97,107,117,101,85,77,122,119,89,81,120,121,113,102,129,83,118,125,94,121,114,73,121,129)
X3 = c(100,99,103,93,95,95,80,116,106,105,90,113,96,98,109,102,100,107,108,95,91,114,93,115,97)
X4 = c(87,100,103,95,88,84,74,102,105,97,88,108,89,78,109,108,102,110,95,90,85,103,80,104,83) 

columns = data.frame(X1,X2,X3,X4)
col_names = colnames(columns)
df_selected = data.frame(Y)
alpha = 0.05
for (i in seq(1,4))
{
  t_values = c()
  p_values = c()
  
  for (x in col_names)
  {
    
    
    df_selected["x"] <- columns[x]
    print("1")
    temp_model = lm(Y~.,data = df_selected)
    print("2")
    t_values = append(t_values,abs(summary(temp_model)[4]$coefficients[length(df_selected),3]))
    print("3")
    p_values = append(p_values,summary(temp_model)[4]$coefficients[length(df_selected),4])
    print("4")
    
    df_selected =  subset(df_selected, select = -c(which(colnames(df_selected)=="x")) )
    print(which(colnames(df_selected)=="x"))
    print("5")
  }
  if (p_values[which.max(t_values)]<=alpha)
  {
    print("if")
    df_selected[col_names[which.max(t_values)]] = columns[col_names[which.max(t_values)]]
    col_names =  col_names[-which.max(t_values)]
  }
}


