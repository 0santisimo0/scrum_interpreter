class UserStory:
    def __init__(self, user_story_id, title, story_type, assigned_to, description, estimation, acceptance):
        self.user_story_id = user_story_id
        self.title = title
        self.story_type = story_type
        self.assigned_to = assigned_to
        self.description = description
        self.estimation = estimation
        self.acceptance = acceptance

    def getView(self):
        return f"{self.user_story_id}: Title: {self.title}, Type: {self.story_type}, Assigned to: {self.assigned_to}, Description: {self.description}, Estimation: {self.estimation}, Acceptance: {self.acceptance}"