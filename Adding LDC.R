#adding LDC to the Table 

ldc_list <- c('Angola', 'Benin', 'Burkina Faso', 'Burundi', 'Central African Republic', 'Chad', 'Comoros', 
              'Democratic Republic of Congo', 'Djibouti', 'Eritrea', 'Ethiopia', 'Gambia', 'Guinea', 
              'Guinea-Bissau', 'Lesotho', 'Liberia', 'Madagascar', 'Malawi', 'Mali', 'Mauritania', 'Mozambique', 
              'Niger', 'Rwanda', 'Senegal', 'Sierra Leone', 'Somalia', 'South Sudan', 'Sudan', 'Togo', 
              'Uganda', 'Tanzania', 'Zambia', 'Afghanistan', 'Bangladesh', 'Cambodia', 
              'Laos', 'Myanmar', 'Nepal', 'East Timor', 'Yemen', 'Haiti', 'Kiribati', 
              'Solomon Islands', 'Tuvalu')

#checking if the list has all values 
length(ldc_list)

#adding a column for least developed countries, 1 if the country is LDC and 0 if not
Final_Table <- Final_Table %>%
  mutate(LDC = case_when(
    Country %in% ldc_list ~ 1,
    TRUE ~ 0
  ))

view(Final_Table)
