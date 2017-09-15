
export function tierClass(tier){
  const lvl = {"Salt": 5, "Silver": 4, "Gold": 3, "Platinum": 2,"Diamond": 1};
  if (tier in lvl){
    return 'icon-tier-' + lvl[tier];
  } else {
    return '';
  }
}