import re
class Solution(object):
    def isPalindrome(self, s):
        """
        :type s: str
        :rtype: bool
        """
        
        s = re.sub(r'[^\w\s]','',s)
        s = s.replace(" ", "")
        s = s.lower()
        
        if len(s) == 0:
            return True
        
        left = 0
        right = len(s)-1

        while (left <= right):
        	if(s[left]!=s[right]):
        		return False
        	left = left + 1
        	right = right - 1

        return True
     