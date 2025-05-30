fn yeah(int32 x): int32{
  if(x!=10) {
    let y= yeah(x+1);
    return y+x;
  } 
  return x;
}

exit(yeah(9));
