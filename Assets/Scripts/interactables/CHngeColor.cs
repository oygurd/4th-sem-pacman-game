using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CHngeColor : MonoBehaviour
{
    private Camera camera;
    private void Awake()
    {
        camera = GetComponent<Camera>();
    }

    // Update is called once per frame
    void Update()
    {
        if (Input.GetMouseButtonDown(0))
        {
            Ray mouseRay = camera.ScreenPointToRay(Input.mousePosition);
            if(Physics.Raycast(mouseRay, out RaycastHit hit))
            {
                Interactable interactable = hit.transform.GetComponent<Interactable>();
                if(interactable)
                {
                    interactable.Interact();
                }    
            }
        }
    }
}
