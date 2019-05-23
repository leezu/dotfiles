alias iids="grep -o 'i-\S*' | paste -sd ' '"  # Get all instance IDs
alias iids_parallel="grep -o 'i-\S*' | paste -sd ' ' | sed 's/ /,/g' | (echo -n '-S ' && cat)"
alias vids="grep -o 'v-\S*' | paste -sd ' '"  # Get all volume IDs