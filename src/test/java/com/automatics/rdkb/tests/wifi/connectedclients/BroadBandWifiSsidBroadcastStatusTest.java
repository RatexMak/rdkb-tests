/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.wifi.connectedclients;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;

public class BroadBandWifiSsidBroadcastStatusTest extends AutomaticsTestBase{
	

    /**
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1: Disable " Broadcast SSID" for 2.4Ghz WiFi network and verify disabled status from device device</li>
     * <li>Expected: The value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled must be set to false</li>
     * <li>Step 2a: Verify the status of Broadcast SSID as "False" using WebPA request</li>
     * <li>Expected: WebPA request for the parameter "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled" should
     * return value as False</li>
     * <li>Step 2b: Disable and enable back the Device.WiFi.SSID.10001.Enable</li>
     * <li>Expected: Device.WiFi.SSID.10001.Enable should return value true</li>
     * <li>Step 3: Verify whether 2.4 GHz SSID is seen in in 2.4Ghz client laptop</li>
     * <li>Expected:2.4Ghz wifi ssid should not be listed in available networks for client device</li>
     * <li>Step 4: Enable "Broadcast SSID" for 2.4Ghz WiFi network and verify status using WebPA request</li>
     * <li>Expected:The value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled must be set to true</li>
     * <li>Step 5a: Check the status of Broadcast SSID as "True" using WebPA request</li>
     * <li>Expected: WebPA request for the parameter "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled" should
     * return value as True</li>
     * <li>Step 5b: Disable and enable back the Device.WiFi.SSID.10001.Enable</li>
     * <li>Expected: Device.WiFi.SSID.10001.Enable should return value true</li>
     * <li>Step 6: Verify whether 2.4 GHz SSID is seen in in 2.4Ghz client laptop</li>
     * <li>Expected:2.4Ghz ssid should be listed in available networks for client device</li>
     * </ol>
     * 
     * @author Revanth.k
	 * @refactor Athira
     * 
     * @param Dut
     *            {@link device}
     */

    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-2001")
    public void testVerifyWifiSsidBroadcastingStatusFor2GhzPrivateSsid(Dut device) {

	// variable to store the testcase id
	String testCaseId = "TC-RDKB-WIFI-201";
	// boolean variable to store the status of each step
	boolean status = false;
	// variable to store the error message
	String errorMessage = null;
	// variable to store the step number
	String step = "s1";
	// variable to store the connected client details
	Dut connectedClientDevice = null;
	// variable to store the status for setting precondition
	boolean statusForPreCondition = false;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-2001");
	LOGGER.info("TEST DESCRIPTION: Enable/Disable Broadcast SSID for 2.4Ghz WiFi network and verify disabled status");

	LOGGER.info("TEST STEPS : ");

	LOGGER.info("1. Disable  Broadcast SSID for 5Ghz WiFi network and verify disabled status from  device");
	LOGGER.info(
		"2a. Verify the status of Broadcast SSID as \"Down\" using WebPA request.");
	LOGGER.info("2b. Disable and enable back the Device.WiFi.SSID.10001.Enable");
	LOGGER.info("3. Verify whether 2.4 GHz SSID is seen in in 5Ghz client laptop");
	LOGGER.info(
		"4. Enable \"Broadcast SSID\" for 2.4Ghz WiFi network and verify status using WebPA request");
	LOGGER.info("5a. Check the status of Broadcast SSID as \"Up\" using WebPA request");
	LOGGER.info("5b. Disable and enable back the Device.WiFi.SSID.10001.Enable");
	LOGGER.info("6. Verify whether 2.4 GHz SSID is seen in in 5Ghz client laptop");

	LOGGER.info("#######################################################################################");

	try {
	    /*
	     * Pre-condition :
	     * 
	     * 1.Find a device which has 2.4GHz Wifi support 2.Set the ssid names in 2.4Ghz and 5Ghz to different values
	     */
	    LOGGER.info("***********EXECUTING PRECONDITIONS*********");
	    LOGGER.info("PreCondition 1:Change the ssid names in 2.4Ghz and 5Ghz to different values");
	    LOGGER.info("PreCondition 2:Get a 2.4Ghz wifi capable client device");
	    // Get a client of device with 2.4GHz radio support
	    statusForPreCondition = BroadBandConnectedClientUtils.setPrivateWifiSsidNamesIn2GhzAnd5GhzToStandardValues(
		    device, tapEnv);
	    if (!statusForPreCondition) {
		errorMessage = "Failed in setting ssid names to different values as Pre Condition";
		LOGGER.error(errorMessage);
		throw new TestException(errorMessage);
	    }
	    LOGGER.info("Waiting for 2 Minutes after changing the Wifi SSID names");
	    tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
	    
	    try{
		 // Get a client of device with 2.4GHz radio support
	    	connectedClientDevice = BroadBandConnectedClientUtils
			    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    }catch(Exception exception){
		errorMessage = exception.getMessage();
		    LOGGER.error("Exception occured during execution = >" + errorMessage);
	    }
	   
	    LOGGER.info("***********Completed Pre Conditions*********");
	    if (connectedClientDevice != null) {

		/*
		 * Step 1: Disable the Private 2.4Ghz SSID Broadcast status via WebPA
		 */
		step = "s1";
		status = false;

		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 1:  DESCRIPTION : Disable \"Private Broadcast SSID status\" for 2.4Ghz WiFi network");
		LOGGER.info("Step 1: ACTION : Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled set to false");
		LOGGER.info("Expected:The value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled must be set to false ");
		LOGGER.info("*****************************************************************************************");

		errorMessage = "Unable to disable 2.4GHz Private SSID Broadcast status using WebPA parameter - Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled";

		// Disable the WebPA parameter "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled"
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());

		LOGGER.info("Step 1: Actual: "
			+ (status ? "The Broadcast SSID status value is set to false for 2.4GHz WiFi network"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		// wait for one minute for ssid advertisement status to turn to false
		LOGGER.info("wait for 90 seconds");
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

		/*
		 * Step 2: Verify the Private 2.4 GHz SSID Broadcast status is disabled or not via WebPA.
		 */

		step = "s2";
		status = false;
	
		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 2a) :  DESCRIPTION :Verify the Private 2.4 GHz SSID Broadcast status is disabled or not via WebPA.");
		LOGGER.info("Step 2a) : ACTION : Get value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled");
		LOGGER.info("Step 2a) : WebPA request for the parameter \"Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled\" should return value as False ");
		
		LOGGER.info("Step 2b) :  DESCRIPTION :Disable and enable back the Device.WiFi.SSID.10001.Enable");
		LOGGER.info("Step 2a) : ACTION : First set Device.WiFi.SSID.10001.Enable with false, then set to value true");
		
		LOGGER.info("Expected: 2a) : Expected :2.4 GHZ private SSID should Disabled and enabled back");
		LOGGER.info("*****************************************************************************************");

		errorMessage = "Broadcast status of 2.4GHz private SSID is not disabled even after disabling via WebPA parameter - Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled";

		/*
		 * Get the value of "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled" value as False
		 */
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.FALSE);
			
		LOGGER.info("Step 2(a): Actual: " + (status ? "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled WebPA parameter returns value as False." : errorMessage));

		status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Waiting for 2 minutes after disabling 2.4GHZ ssid");
		tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
		LOGGER.info("Response of webpa disable command " + status);
		if (status)
		{
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Step 2(b): Actual: " + (status ? "Disabled and enabled back the 2.4GHz SSID" : errorMessage));
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		/*
		 * Step 3: Verify whether 2.4 GHz SSID is seen in 2.4Ghz client laptop
		 */

		// Wait for some time to reflect the changes in client device
		LOGGER.info("Waiting for five minutes");
		tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);

		step = "s3";
		status = false;
		
		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 3:  DESCRIPTION : Verify whether 2.4 GHz SSID is seen in in 2.4Ghz client laptop");
		LOGGER.info("Step 3: ACTION : Execute the command and check for presence of 2.4GHz SSID in command output.");
		LOGGER.info("Step 3: Expected: 2.4 GHz SSID is seen in in client laptop ");
		LOGGER.info("*****************************************************************************************");

		/*
		 * Verify 2.4GHz is not available in the client device after disabling the SSID Advertisement Enabled
		 * value.
		 */

		String ssIdName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_2_GHZ);
		LOGGER.info("SSID name in 2.4Ghz band: " + ssIdName);

		errorMessage = "2.4 GHz is still available in the client device even after disabling the SSIDAdvertismentEnabledValue";

		try {

		    status = BroadBandConnectedClientUtils.verifySsidVisibilityInClientDevice(connectedClientDevice,
			    tapEnv, ssIdName, false);
				
			LOGGER.info("Step 3: Actual: " + (status ? "Successfully verified 2.4 GHz SSID is seen in in 2.4Ghz client laptop" : errorMessage));
		} catch (Exception exception) {
		    LOGGER.error("Exception occurred during execution => " + exception.getMessage());
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		/*
		 * Step 4: Enable "Private Broadcast SSID status" for 2.4Ghz WiFi network
		 */

		step = "s4";
		status = false;		
		
		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 4: DESCRIPTION : Enable \"Private Broadcast SSID status\" for 2.4Ghz WiFi network");
		LOGGER.info("Step 4: ACTION : Execute the command and check for presence of 2.4GHz SSID in command output.");
		LOGGER.info("Step 4: Expected: The value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled must be set to true ");
		LOGGER.info("*****************************************************************************************");

		errorMessage = "Not able to enabled 2.4 GHz SSID using Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled WEBPA parameter ";
		/*
		 * Enable SSID value for 2.4GHz network
		 */
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Step 4: Actual: "
			+ (status ? "Broadcast status is set to true for 2.4GHz WiFi network"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		// wait for one minute for ssid advertisement status to turn to true
		LOGGER.info("wait for one minute");
		tapEnv.waitTill(AutomaticsConstants.ONE_MINUTE_IN_MILLIS);

		/*
		 * Step 5: Verify the status of Broadcast SSID using WebPA request
		 */

		step = "s5";
		status = false;
		
		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 5a) :  DESCRIPTION :Verify the Private 2.4 GHz SSID Broadcast status is disabled or not via WebPA.");
		LOGGER.info("Step 5a) : ACTION : Get value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled");
		LOGGER.info("Step 5a) : WebPA request for the parameter \"Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled\" should return value as True ");
		
		LOGGER.info("Step 5b) :  DESCRIPTION :Disable and enable back the Device.WiFi.SSID.10001.Enable");
		LOGGER.info("Step 5b) : ACTION : First set Device.WiFi.SSID.10001.Enable with false, then set to value true");
		
		LOGGER.info("Expected: 5b) : Expected :2.4 GHZ private SSID should Disabled and enabled back");
		LOGGER.info("*****************************************************************************************");

		errorMessage = "Broadcast SSID status obtained using WebPA Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled is NOT TRUE";

		/*
		 * Get the value of "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled" value as True
		 */
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.TRUE);
		LOGGER.info("Step 5(a): Actual: Status of 2.4GHz SSID broadcast is " + status );

		status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Waiting for 2 minutes after disabling 2.4GHZ ssid");
		tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
		if(status)
		{
		LOGGER.info(" SSID Broadcast is disabled");
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Step 5(b): Actual: " + (status ? "Disabled and enabled back the 2.4GHz SSID" : errorMessage));
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		/*
		 * Step 6: Verify whether 2.4 GHz SSID is seen in 2.4Ghz client device
		 */

		// Wait for some time to reflect the changes in client device
		LOGGER.info("Waiting for five minutes");
		tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);

		step = "s6";
		status = false;		
		
		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 6:  DESCRIPTION : Verify whether 2.4 GHz SSID is seen in in 2.4Ghz client laptop");
		LOGGER.info("Step 6: ACTION : Execute the command and check for presence of 2.4GHz SSID in command output.");
		LOGGER.info("Step 6: Expected: 2.4 GHz SSID is seen in in client laptop ");
		LOGGER.info("*****************************************************************************************");
		/*
		 * Verify 2.4GHz is seen in client devices after enabling the above value
		 */
		errorMessage = "2.4 GHz is not available in the client device network after enabling the SSIDAdvertismentEnabledValue";

		try {

		    status = BroadBandConnectedClientUtils.verifySsidVisibilityInClientDevice(connectedClientDevice,
			    tapEnv, ssIdName, true);
		    
		    LOGGER.info("Step 6: Actual: " + (status ? "Successfully verified 2.4 GHz SSID is seen in client device" : errorMessage));
		} catch (Exception exception) {
		    LOGGER.error("Exception occurred during execution => " + exception.getMessage());
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
	    } else {
		errorMessage = "Failed to find device with 2.4GHz Wifi support";
		LOGGER.error(errorMessage);
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
	    }

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured during execution = >" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	} finally {
	    // Enable the WebPA parameter "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled"
	    BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
		    BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-2001");
    }
    
    /**
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1: Disable " Broadcast SSID" for 5Ghz WiFi network and verify disabled status from  device</li>
     * <li>Expected: The value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled must be set to false</li>
     * <li>Step 2a: Verify the status of Broadcast SSID as "Down" using WebPA request</li>
     * <li>Expected: WebPA request for the parameter "Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled" should
     * return value as False</li>
     * <li>Step 2b: Disable and enable back the Device.WiFi.SSID.10101.Enable</li>
     * <li>Expected: Device.WiFi.SSID.10101.Enable should return value true</li>
     * <li>Step 3: Verify whether 5 GHz SSID is seen in in 5Ghz client laptop</li>
     * <li>Expected:5Ghz wifi ssid should not be listed in available networks for client device</li>
     * <li>Step 4: Enable "Broadcast SSID" for 5Ghz WiFi network and verify status using WebPA request</li>
     * <li>Expected:The value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled must be set to true</li>
     * <li>Step 5a: Check the status of Broadcast SSID as "True" using WebPA request</li>
     * <li>Expected: WebPA request for the parameter "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled" should
     * return value as True</li>
     * <li>Step 5b: Disable and enable back the Device.WiFi.SSID.10101.Enable</li>
     * <li>Expected: Device.WiFi.SSID.10101.Enable should return value true</li>
     * <li>Step 6: Verify whether 5 GHz SSID is seen in in 5Ghz client laptop</li>
     * <li>Expected:5Ghz ssid should be listed in available networks for client device</li>
     * </ol>
     * 
     * @author Revanth.K
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-2002")
    public void testVerifyBroadBandWifiSsidStatusFor5GHz(Dut device) {

	// variable to store the testcase id
	String testCaseId = "TC-RDKB-WIFI-202";
	// boolean variable to store the status of each step
	boolean status = false;
	// variable to store the error message
	String errorMessage = null;
	// variable to store the step number
	String step = "s1";
	// variable to store the connected client details
	Dut connectedClientDevice = null;
	//variable to store the status for setting precondition
    boolean statusForPreCondition = false;

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-2002");
	LOGGER.info("TEST DESCRIPTION: Enable/Disable Broadcast SSID for 5Ghz WiFi network and verify disabled status");

	LOGGER.info("TEST STEPS : ");

	LOGGER.info("1. Disable  Broadcast SSID for 5Ghz WiFi network and verify disabled status from  device");
	LOGGER.info(
		"2a. Verify the status of Broadcast SSID as \"Down\" using WebPA request.");
	LOGGER.info("2a. Check the status of Broadcast SSID as \"Up\" using WebPA request");
	LOGGER.info("2b. Disable and enable back the Device.WiFi.SSID.10101.Enable");
	LOGGER.info("3. Verify whether 5 GHz SSID is seen in in 5Ghz client laptop");
	LOGGER.info(
		"4. Enable \"Broadcast SSID\" for 5Ghz WiFi network and verify status using WebPA request");
	LOGGER.info("5a. Check the status of Broadcast SSID as \"Up\" using WebPA request");
	LOGGER.info("5b. Disable and enable back the Device.WiFi.SSID.10101.Enable");
	LOGGER.info("6. Verify whether 5 GHz SSID is seen in in 5Ghz client laptop");

	LOGGER.info("#######################################################################################");

	try {

	    /*
	     * Pre-condition : 
	     * 1.Find a device which has 5GHz Wifi support
	     * 2.Set the ssid names in 2.4Ghz and 5Ghz to different values
	     * 
	     */
	    LOGGER.info("***********EXECUTING PRECONDITIONS*********");
	    LOGGER.info("PreCondition 1:Change the ssid names in 2.4Ghz and 5Ghz to different values");
	    LOGGER.info("PreCondition 2:Get a 5Ghz wifi capable client device");
	    statusForPreCondition = BroadBandConnectedClientUtils.setPrivateWifiSsidNamesIn2GhzAnd5GhzToStandardValues(device, tapEnv);
	    if(!statusForPreCondition)
	    {
		errorMessage = "Failed in setting ssid names to different values as Pre Condition";
		LOGGER.error(errorMessage);
		throw new TestException(errorMessage);
	    }
	    LOGGER.info("waiting for two minutes after changing the ssid names");
	    tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
		LOGGER.info("PRE-CONDITION: Actual: "
				+ (statusForPreCondition ? "ssid names in 2.4Ghz and 5Ghz changed to different values"
					: errorMessage));
		connectedClientDevice = BroadBandConnectedClientUtils.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
	    LOGGER.info("***********Completed PRECONDITIONS*********");
	    if (connectedClientDevice != null) {

		/*
		 * Step 1: Disable the Private 5Ghz SSID Broadcast status via WebPA
		 */

		step = "s1";
		status = false;

		String ssIdName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_5_GHZ);

		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 1:  DESCRIPTION : Disable \"Private Broadcast SSID status\" for 5Ghz WiFi network");
		LOGGER.info("Step 1: ACTION : Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled set to false");
		LOGGER.info("Expected:The value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled must be set to false ");
		LOGGER.info("*****************************************************************************************");

		errorMessage = "Unable to disable 5GHz Private SSID Broadcast status using WebPA parameter - Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled";

		LOGGER.info("SSID name for 5Ghz band: " + ssIdName);

		/*
		 * Disable Private Broadcast SSID status value for 5GHz network
		 */
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());

		LOGGER.info("Actual: "
			+ (status ? "The Broadcast SSID status value is set to false for 5GHz WiFi network"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		// wait for one minute for ssid advertisement status to turn to false
		LOGGER.info("wait for 90 seconds ");
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

		/*
		 * Step 2: Verify the Private 5 GHz SSID Broadcast status is disabled or not via WebPA.
		 */

		step = "s2";
		status = false;
		
		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 2a) :  DESCRIPTION :Verify the Private 5 GHz SSID Broadcast status is disabled or not via WebPA.");
		LOGGER.info("Step 2a) : ACTION : Get value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled");
		LOGGER.info("Step 2a) : WebPA request for the parameter \"Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled\" should return value as False ");
		
		LOGGER.info("Step 2b) :  DESCRIPTION :Disable and enable back the Device.WiFi.SSID.10101.Enable");
		LOGGER.info("Step 2b) : ACTION : First set Device.WiFi.SSID.10101.Enable with false, then set to value true");
		
		LOGGER.info("Expected: 2b) : Expected :5 GHZ private SSID should Disabled and enabled back");
		LOGGER.info("*****************************************************************************************");

		errorMessage = "Broadcast status of 5GHz private SSID is not disabled even after disabling via WebPA parameter - Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled";

		/*
		 * Get the value of "Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled" value as False
		 */
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.FALSE);
		
		LOGGER.info("Step 2(a): Actual: " + (status ? "Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled WebPA parameter returns value as False." : errorMessage));
		
		status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Waiting for 2 minutes after disabling 5GHZ ssid");
		tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
		LOGGER.info("status of webpa disable command is : " + status );
		if (status)
		{
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());

		LOGGER.info("Step 2(b): Actual: " + (status ? "Disabled and enabled back 5 GHz SSID Broadcast" : errorMessage));
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		/*
		 * Step 3: Verify whether 5 GHz SSID is seen in in 5Ghz client device
		 */

		// Wait for some time to reflect the changes in client device
		LOGGER.info("Waiting for 5 minutes");
		tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);

		step = "s3";
		status = false;
		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 3:  DESCRIPTION : Verify whether 5 GHz SSID is seen in in 2.4Ghz client laptop");
		LOGGER.info("Step 3: ACTION : Execute the command and check for presence of 2.4GHz SSID in command output.");
		LOGGER.info("Step 3: Expected: 5 GHz SSID is seen in client laptop ");
		LOGGER.info("*****************************************************************************************");

		/*
		 * Verify 5GHz is not available in the client device after disabling the SSID Advertisement Enabled
		 * value.
		 */
		errorMessage = "5 GHz is still available in the client device even after disabling the SSIDAdvertismentEnabledValue";
		try {

		    status = BroadBandConnectedClientUtils.verifySsidVisibilityInClientDevice(connectedClientDevice,
			    tapEnv, ssIdName, false);
		    
		    LOGGER.info("Step 3: Actual: " + (status ? "Successfully verified 5 GHz SSID is seen in in 2.4Ghz client laptop" : errorMessage));

		} catch (Exception exception) {
		    LOGGER.error("Exception occurred during execution => " + exception.getMessage());
		}

		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		/*
		 * Step 4: Enable "Private Broadcast SSID status" for 5Ghz WiFi network
		 */

		step = "s4";
		status = false;
		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 4: DESCRIPTION : Enable \"Private Broadcast SSID status\" for 5Ghz WiFi network");
		LOGGER.info("Step 4: ACTION : Execute the command and check for presence of 5GHz SSID in command output.");
		LOGGER.info("Step 4: Expected: The value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled must be set to true ");
		LOGGER.info("*****************************************************************************************");

		errorMessage = "Not able to enabled 5 GHz SSID using Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled WEBPA parameter";
		/*
		 * Enable SSID value for 5GHz network
		 */

		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Step 4: Actual: "
			+ (status ? "The Private Broadcast SSID status value is set to true for 5GHz WiFi network"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		// wait for one minute for ssid advertisement status to turn to true
		LOGGER.info("wait for 90 seconds");
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

		/*
		 * Step 5: Verify the status of Broadcast SSID using WebPA request
		 */

		step = "s5";
		status = false;
		
		
		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 5a) :  DESCRIPTION :Verify the Private 5 GHz SSID Broadcast status is disabled or not via WebPA.");
		LOGGER.info("Step 5a) : ACTION : Get value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled");
		LOGGER.info("Step 5a) : WebPA request for the parameter \"Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled\" should return value as True ");
		
		LOGGER.info("Step 5b) :  DESCRIPTION :Disable and enable back the Device.WiFi.SSID.10101.Enable");
		LOGGER.info("Step 5b) : ACTION : First set Device.WiFi.SSID.10101.Enable with false, then set to value true");
		
		LOGGER.info("Expected: 5b) : Expected :5 GHZ private SSID should Disabled and enabled back");
		LOGGER.info("*****************************************************************************************");

		errorMessage = "Broadcast SSID status obtained using WebPA parameter Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled is NOT TRUE";

		/*
		 * Get the value of "Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled" value as True
		 */
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.TRUE);
		LOGGER.info("Step 5(a): Actual: Status of 5GHz SSID broadcast is " + status );

		BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Waiting for 2 minutes after disabling 5GHZ ssid");
		tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
		if(status)
		{
		LOGGER.info(" SSID Broadcast is disabled");
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Step 5(b): Actual: " + (status ? "Disabled and enabled back the 5GHz SSID" : errorMessage));
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		/*
		 * Step 6: Verify whether 5 GHz SSID is seen in client device after enabling the above value
		 */

		// Wait for some time to reflect the changes in client device
		LOGGER.info("Waiting for five minutes");
		tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);

		step = "s6";
		status = false;
		
		
		LOGGER.info("*****************************************************************************************");
		LOGGER.info("Step 6:  DESCRIPTION : Verify whether 5 GHz SSID is seen in in 2.4Ghz client laptop");
		LOGGER.info("Step 6: ACTION : Execute the command and check for presence of 2.4GHz SSID in command output.");
		LOGGER.info("Step 6: Expected: 5 GHz SSID is seen in client laptop ");
		LOGGER.info("*****************************************************************************************");

		/*
		 * Verify 5GHz is seen in client devices after enabling the above value
		 */
		errorMessage = "5 GHz is not available in the client device network after enabling the SSIDAdvertismentEnabledValue";

		try {

		    status = BroadBandConnectedClientUtils.verifySsidVisibilityInClientDevice(connectedClientDevice,
			    tapEnv, ssIdName, true);
		    LOGGER.info("Step 6: Actual: " + (status ? "Successfully verified 5 GHz SSID is seen in client device" : errorMessage));

		} catch (Exception exception) {
		    LOGGER.error("Exception occurred during execution => " + exception.getMessage());
		}

		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
	    } else {
		errorMessage = "Failed to find client device with 5GHz wifi support";
		LOGGER.error(errorMessage);
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
	    }

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(errorMessage);
	    LOGGER.error("Exception occured while enabling/ disabling the SSID Advertisement value for 5 GHz network"
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	} finally {
	    // Enable the WebPA parameter "Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled"
	    status = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
		    BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());

	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-2002");
    }   

}
