# Embodiment Layer for Distributed Cognitive Mesh

This directory contains the embodiment layer interfaces that enable the Distributed Agentic Cognitive Grammar Network to interact with physical and virtual environments through Unity3D, ROS, and WebSocket APIs.

## Overview

The embodiment layer provides real-time bidirectional communication between the cognitive fusion reactor and external agents, enabling embodied cognition and distributed intelligence across multiple platforms.

## Components

### Unity3D Integration (`unity3d/`)
- **Cognitive Behavior Scripts**: C# components for cognitive agent control
- **Real-time Attention Visualization**: 3D visualization of attention allocation
- **Cognitive State Representation**: Visual representation of tensor states
- **Interactive Agent Controllers**: User interaction with cognitive agents

### ROS Integration (`ros/`)
- **Cognitive Planning Services**: ROS services for cognitive task planning
- **Attention-based Navigation**: Navigation using cognitive attention mechanisms
- **Multi-robot Coordination**: Distributed cognitive coordination protocols
- **Sensor Data Processing**: Cognitive processing of robotic sensor data

### WebSocket API (`websocket/`)
- **Real-time Communication**: Bidirectional WebSocket protocols
- **Cognitive State Streaming**: Live cognitive state updates
- **Agent Registration**: Dynamic agent connection management
- **Event Broadcasting**: Cognitive event distribution

### REST API (`rest/`)
- **Cognitive State Endpoints**: HTTP APIs for cognitive state access
- **Task Submission**: RESTful task processing interfaces
- **Attention Queries**: Attention allocation query endpoints
- **Agent Management**: CRUD operations for cognitive agents

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    External Agents                         │
│  (Unity3D Games, ROS Robots, Web Applications)             │
├─────────────────────────────────────────────────────────────┤
│                   Embodiment Layer                         │
│  ┌─────────────┬─────────────┬─────────────┬─────────────┐  │
│  │   Unity3D   │     ROS     │  WebSocket  │  REST API   │  │
│  │ Integration │ Integration │    API      │    Server   │  │
│  └─────────────┴─────────────┴─────────────┴─────────────┘  │
├─────────────────────────────────────────────────────────────┤
│                Cognitive Fusion Reactor                    │
│  (AtomSpace + ECAN + ggml + Meta-Cognition)                │
└─────────────────────────────────────────────────────────────┘
```

## API Specifications

### REST Endpoints

#### Cognitive State
- `GET /api/v1/cognitive/state` - Get current cognitive state
- `GET /api/v1/cognitive/tensor/{id}` - Get specific tensor state
- `POST /api/v1/cognitive/process` - Submit cognitive task
- `PUT /api/v1/cognitive/attention` - Update attention allocation

#### Agent Management
- `POST /api/v1/agents/register` - Register new cognitive agent
- `GET /api/v1/agents/{id}` - Get agent information
- `PUT /api/v1/agents/{id}` - Update agent configuration
- `DELETE /api/v1/agents/{id}` - Unregister agent

#### Attention Allocation
- `GET /api/v1/attention/allocation` - Get current attention distribution
- `GET /api/v1/attention/history` - Get attention allocation history
- `POST /api/v1/attention/focus` - Set attention focus target

### WebSocket Events

#### Cognitive State Updates
- `cognitive.state.update` - Real-time cognitive state changes
- `cognitive.tensor.modified` - Tensor modification events
- `cognitive.pattern.detected` - Pattern recognition events

#### Attention Events
- `attention.allocation.change` - Attention distribution changes
- `attention.focus.shift` - Attention focus transitions
- `attention.resource.competition` - Resource competition events

#### Agent Events
- `agent.registration` - New agent connection
- `agent.disconnection` - Agent disconnection
- `agent.status.update` - Agent status changes

## Unity3D Integration

### Cognitive Behavior Components

```csharp
public class CognitiveAgent : MonoBehaviour
{
    public CognitiveState currentState;
    public AttentionVisualizer attentionViz;
    public WebSocketClient cognitiveClient;
    
    public void UpdateCognitiveState(CognitiveStateEvent evt)
    {
        currentState = evt.state;
        attentionViz.UpdateVisualization(evt.attention);
        ApplyCognitiveBehavior(evt.actions);
    }
    
    public void SubmitCognitiveTask(string task)
    {
        cognitiveClient.Send(new CognitiveTaskMessage {
            agentId = gameObject.GetInstanceID(),
            task = task,
            priority = GetCurrentPriority()
        });
    }
}
```

### Attention Visualization

```csharp
public class AttentionVisualizer : MonoBehaviour
{
    public void UpdateVisualization(AttentionState attention)
    {
        // Visualize attention allocation as heat map
        var heatMap = GenerateAttentionHeatMap(attention);
        ApplyHeatMapToMaterial(heatMap);
        
        // Update particle systems for attention flow
        UpdateAttentionParticles(attention.flow);
    }
}
```

## ROS Integration

### Cognitive Planning Service

```cpp
class CognitivePlannerService
{
public:
    bool planCognitiveTask(
        cognitive_msgs::PlanRequest& request,
        cognitive_msgs::PlanResponse& response)
    {
        // Submit task to cognitive fusion reactor
        auto cognitiveResult = submitToCognitiveFusion(request.task);
        
        // Convert cognitive plan to ROS navigation plan
        response.plan = convertToROSPlan(cognitiveResult);
        response.confidence = cognitiveResult.confidence;
        
        return true;
    }
};
```

### Multi-Robot Coordination

```cpp
class CognitiveCoordinator
{
private:
    ros::ServiceClient cognitive_client_;
    std::vector<RobotAgent> robots_;
    
public:
    void coordinateRobots()
    {
        // Get distributed attention allocation
        auto attention = getCognitiveAttention();
        
        // Assign tasks based on cognitive priorities
        for (auto& robot : robots_) {
            auto task = attention.getTaskForAgent(robot.id);
            robot.assignTask(task);
        }
    }
};
```

## Performance Requirements

### Latency Targets
- **REST API Response**: <50ms for simple queries
- **WebSocket Latency**: <10ms for real-time updates
- **Unity3D Frame Rate**: Maintain 60+ FPS with cognitive processing
- **ROS Service Calls**: <100ms for cognitive planning requests

### Throughput Targets
- **Concurrent Connections**: 1000+ WebSocket connections
- **API Requests**: 10,000+ requests/second
- **Unity3D Agents**: 100+ cognitive agents per scene
- **ROS Topics**: 1000+ messages/second

### Reliability Targets
- **Uptime**: 99.9% availability
- **Error Rate**: <0.1% for API requests
- **Connection Stability**: <1% WebSocket disconnection rate
- **Data Integrity**: 100% message delivery guarantee

## Security

### Authentication
- JWT token-based authentication for REST APIs
- Certificate-based authentication for WebSocket connections
- ROS node authentication through cognitive credentials

### Authorization
- Role-based access control for cognitive operations
- Agent-specific permission management
- Attention allocation access controls

### Data Protection
- TLS encryption for all network communications
- Cognitive state data encryption at rest
- Privacy-preserving attention allocation

## Testing

### Integration Testing
- End-to-end Unity3D cognitive agent scenarios
- Multi-robot ROS coordination validation
- WebSocket connection stability testing
- REST API load testing

### Performance Testing
- Latency measurement under various loads
- Throughput benchmarking
- Memory usage profiling
- Connection scaling tests

### Cognitive Testing
- Embodied cognition behavior validation
- Attention allocation accuracy testing
- Real-world scenario validation
- Cognitive-physical interaction testing

---

*The embodiment layer enables the cognitive fusion reactor to transcend digital boundaries and engage with the physical world through intelligent, adaptive interfaces.*