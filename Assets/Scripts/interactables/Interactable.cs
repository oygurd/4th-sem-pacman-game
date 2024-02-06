using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Interactable : MonoBehaviour
{
    public void Interact()
    {
        MeshRenderer renderer = GetComponent<MeshRenderer>();
        renderer.material.SetColor("_BaseColor", Random.ColorHSV());
    }
}
