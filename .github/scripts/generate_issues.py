#!/usr/bin/env python3
"""
Generate GitHub issues from open issues documentation structure.
This script parses the nested structure and creates issues with actionable steps.
"""

import os
import re
import json
import requests
import yaml
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass
from urllib.parse import urlparse

@dataclass
class IssueItem:
    """Represents an issue item from the documentation."""
    title: str
    url: str
    level: int
    parent: Optional[str] = None
    children: List[str] = None
    
    def __post_init__(self):
        if self.children is None:
            self.children = []

class IssueGenerator:
    def __init__(self, github_token: Optional[str] = None):
        self.github_token = github_token or os.getenv('GITHUB_TOKEN')
        self.repo_owner = os.getenv('GITHUB_REPOSITORY_OWNER', 'gnu')
        self.repo_name = os.getenv('GITHUB_REPOSITORY', 'gnu/hurd')
        self.api_base = f"https://api.github.com/repos/{self.repo_owner}/{self.repo_name}"
        
    def parse_documentation_structure(self) -> List[IssueItem]:
        """Parse the open issues documentation structure."""
        
        # This is the structure from the provided documentation
        structure_data = {
            "advantages": {"url": "https://www.gnu.org/software/hurd/advantages.html", "level": 0}
        }
        
        return self._flatten_structure(structure_data)
    
    def _flatten_structure(self, structure: Dict, parent: str = None) -> List[IssueItem]:
        """Flatten the nested structure into a list of IssueItem objects."""
        items = []
        
        for key, data in structure.items():
            if isinstance(data, dict) and 'url' in data:
                item = IssueItem(
                    title=self._format_title(key),
                    url=data['url'],
                    level=data.get('level', 0),
                    parent=parent
                )
                items.append(item)
                
                # Process children recursively
                if 'children' in data:
                    child_items = self._flatten_structure(data['children'], key)
                    items.extend(child_items)
                    
        return items
    
    def _format_title(self, key: str) -> str:
        """Format the key into a readable title."""
        return key.replace('_', ' ').title()
    
    def generate_actionable_steps(self, item: IssueItem) -> List[str]:
        """Generate actionable steps for an issue based on its type and level."""
        steps = []
        
        # Common steps for all items
        steps.append(f"Review the documentation at: {item.url}")
        steps.append("Analyze current implementation status")
        steps.append("Identify gaps and missing components")
        
        return steps
    
    def create_issue_body(self, item: IssueItem) -> str:
        """Create the body content for a GitHub issue."""
        steps = self.generate_actionable_steps(item)
        
        body = f"""## Documentation Item: {item.title}

**Source URL:** {item.url}
**Level:** {item.level}
**Parent:** {item.parent if item.parent else 'None'}

### Description
This issue tracks the documentation and implementation work needed for: **{item.title}**

### Actionable Steps
{chr(10).join(f"- [ ] {step}" for step in steps)}
"""
        
        return body
    
    def create_github_issue(self, item: IssueItem) -> Optional[Dict]:
        """Create a GitHub issue for the given item."""
        if not self.github_token:
            print(f"Would create issue: {item.title}")
            return None
        
        headers = {
            'Authorization': f'token {self.github_token}',
            'Accept': 'application/vnd.github.v3+json'
        }
        
        data = {
            'title': f"Documentation: {item.title}",
            'body': self.create_issue_body(item),
            'labels': ['documentation', 'enhancement', 'automated-issue']
        }
        
        try:
            response = requests.post(
                f"{self.api_base}/issues",
                headers=headers,
                json=data,
                timeout=30
            )
            
            if response.status_code == 201:
                print(f"Created issue: {item.title}")
                return response.json()
            else:
                print(f"Failed to create issue for {item.title}: {response.status_code}")
                return None
                
        except Exception as e:
            print(f"Error creating issue for {item.title}: {e}")
            return None
    
    def run(self):
        """Main execution method."""
        print("Parsing open issues documentation structure...")
        items = self.parse_documentation_structure()
        
        print(f"Found {len(items)} items to process")
        
        # Create issues for each item
        created_issues = []
        for item in items:
            issue = self.create_github_issue(item)
            if issue:
                created_issues.append(issue)
        
        print(f"Successfully created {len(created_issues)} issues")
        
        # Save the structure to a JSON file for reference
        output_file = 'open_issues_structure.json'
        try:
            with open(output_file, 'w', encoding='utf-8') as f:
                json.dump([{
                    'title': item.title,
                    'url': item.url,
                    'level': item.level,
                    'parent': item.parent,
                    'actionable_steps': self.generate_actionable_steps(item)
                } for item in items], f, indent=2, ensure_ascii=False)
            
            print(f"Saved structure to {output_file}")
        except (IOError, OSError) as e:
            print(f"Error saving structure to file: {e}")

if __name__ == "__main__":
    generator = IssueGenerator()
    generator.run()