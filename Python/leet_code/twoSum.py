class Solution(object):
    def twoSum(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: List[int]
        """
        
        ### Brute force: O(n^2)rt; O(1)s
        # leng = len(nums)
        # for i in range(0,leng):
        #     for j in range(i+1, leng):
        #         if (nums[i] + nums[j] == target):
        #             return [i, j]
                    
        ### Using dictionary
        map = {}
        for i in range(len(nums)):
            x = nums[i]    
            if(map.has_key(target-x)):
                return [map[target-x], i]
            else:
                map[x] = i

        