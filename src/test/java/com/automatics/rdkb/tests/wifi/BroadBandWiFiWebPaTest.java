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
package com.automatics.rdkb.tests.wifi;

/**
 * Test class with test case for validating TR-181 Parameters using WebPA Protocol
 * 
 * @author Prabhakaran ,Joseph_Maduram
 * @Refactor Athira ,Alan_Bivera
 */
import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWiFiWebPaTest extends AutomaticsTestBase {

    /**
     * This test case is to check the Security modes on 5.0Ghz Private Network
     * 
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10101.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     *
     * @Refactor Athira
     * @param device
     *            device to be used
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1005", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10101.Security.ModeEnabled using WebPA ")
    public void testVerifyAllSupportedSecurityModeFor5GhzPrivateNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-105";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED);
    }

    /**
     * This test case is to check the Security mode on 5.0Ghz Network
     * 
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10102.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     *
     * @Refactor Athira
     * @param device
     *            device to be used
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1006", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10102.Security.ModeEnabled using WebPA ")
    public void testVerifyAllSupportedSecurityModeFor5GhzNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-106";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_SECURITY_MODEENABLED);
    }

    /**
     * 
     * Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10103.Security.ModeEnabled
     * 
     * This test case is to check the Security mode on 5.0Ghz Public Network
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10103.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     * 
     * @Refactor Alan_Bivera
     * @param device
     *            Device to be used
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1007", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10103.Security.ModeEnabled using WebPA")
    public void testVerifyAllSupportedSecurityModeFor5GhzPublicNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-107";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PUBLIC_SECURITY_MODEENABLED);
    }

    /**
     * 
     * Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10001.Security.ModeEnabled
     * 
     * This test case is to check the Security mode on 2.4Ghz Private Network
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10001.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     * 
     * @Refactor Alan_Bivera
     * @param device
     *            Device to be used
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1008", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10001.Security.ModeEnabled using WebPA")
    public void testVerifyAllSupportedSecurityModeFor2_4GhzPrivateNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-108";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED);
    }

    /**
     * 
     * Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10002.Security.ModeEnabled
     * 
     * This test case is to check the Security mode on 2.4Ghz Network
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10002.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     * 
     * @Refactor Alan_Bivera
     * @param device
     *            Device to be used
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1009", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10002.Security.ModeEnabled using WebPA")
    public void testVerifyAllSupportedSecurityModeFor2_4GhzNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-109";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_SECURITY_MODEENABLED);
    }

    /**
     * 
     * Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10003.Security.ModeEnabled
     * 
     * This test case is to check the Security mode on 2.4Ghz Public Network
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10003.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     * 
     * @Refactor Alan_Bivera
     * @param device
     *            Device to be used
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1010", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10003.Security.ModeEnabled using WebPA")
    public void testVerifyAllSupportedSecurityModeFor2_4GhzPublicNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-110";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PUBLIC_SECURITY_MODEENABLED);
    }

    /**
     * This test case checks the SSID on 5Ghz Private network by comparing the output from WebPA with the output from
     * the device
     * <ol>
     * <li>STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10101.SSID to retrieve 5GHz Private SSID name by WebPA.
     * </li>
     * 
     * <li>STEP 2:Verify whether the system command provides the 5GHz private SSID name and verify whether it is
     * matching with the WebPA response</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1011", testDecription = "Verification of 5Ghz Private network SSID name 'Device.WiFi.SSID.10101.SSID' using WebPA")
    public void testValidateSsidNameFor5GhzPrivateNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-111";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;

	try {
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE_CONDITION: Check and Enable 5GHz Private SSID 'Device.WiFi.SSID.10101.Enable' using WebPA.");
	    LOGGER.info("#####################################################################################");
	    if (!BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :NOT ABLE TO ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }
	    /**
	     * STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10101.SSID to retrieve 5GHz Private SSID name by
	     * WebPA.
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10101.SSID to retrieve 5GHz Private SSID name by WebPA.");
	    LOGGER.info("EXPECTED: should be able to get SSID Value by WebPA");
	    LOGGER.info("#####################################################################################");
	    String ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID);
	    LOGGER.info("5GHz private SSID name retrieved using WebPa = " + ssidNameFromWebPa);
	    status = CommonMethods.isNotNull(ssidNameFromWebPa);

	    errorMessage = "Unable to retrieve the 5GHz Private SSID name 'Device.WiFi.SSID.10101.SSID' using WebPA command.";
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully retrieved the 5GHz Private SSID name 'Device.WiFi.SSID.10101.SSID' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 2:Verify whether the system command provides the 5GHz private SSID name and verify whether it is
	     * matching with WebPA response
	     */

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verify whether the system command provides the 5GHz private SSID name and verify whether it is matching with the WebPA response");
	    LOGGER.info(
		    "EXPECTED: should be able to get SSID Name inside the device and the output should match with webpa output");
	    LOGGER.info("#####################################################################################");
	    String interfaceName = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
		    BroadBandTestConstants.BAND_5GHZ, BroadBandTestConstants.PRIVATE_WIFI_TYPE,
		    BroadBandTestConstants.SSID_PARAM);
	    errorMessage = "Seems like Device command or Wi-Fi Interface used for retrieving 5GHz private SSID name is empty or null";
	    if (CommonMethods.isNotNull(interfaceName)) {
		String ssidObtainiedFromDevice = BroadBandWebPaUtils.getSsidNameRetrievedUsingDeviceCommand(device,
			tapEnv, interfaceName);
		LOGGER.info("5GHz private SSID name retrieved using device command =" + ssidObtainiedFromDevice);

		if (CommonMethods.isNotNull(ssidObtainiedFromDevice)) {
		    status = ssidObtainiedFromDevice.contains(ssidNameFromWebPa);
		    errorMessage = "5GHz Private SSID name using device command doesn't matches with name retrieved from WebPA command. ACTUAL : SSID name from device command = "
			    + ssidObtainiedFromDevice
			    + " , SSID name 'Device.WiFi.SSID.10101.SSID' using WebPA command = " + ssidNameFromWebPa;
		} else {
		    status = false;
		    errorMessage = "Unable to retrieve the 5GHz private SSID name using device command.";
		}

	    }

	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully verified the 5GHz private SSID name retrieved from WebPA with device command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while validating  5Ghz Private SSID name 'Device.WiFi.SSID.10101.SSID' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}
    }

    /**
     * This test case checks the SSID on 5Ghz Public network by comparing the output from webpa with the output from the
     * device
     * <ol>
     * <li>STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10103.SSID to retrieve 5GHz Public SSID name by WebPA
     * </li>
     * 
     * <li>STEP 2:Verify whether the system command provides the 5GHz Public SSID name and verify whether it is matching
     * with WebPA response.</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1013", testDecription = "Verification of 5Ghz Public network SSID name Device.WiFi.SSID.10103.SSID using WebPA")
    public void testValidateSsidNameFor5GhzPublicNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-113";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// stores the status of PublicWifi
	boolean isPublicWifiConfigurationSet = false;
	// stores the status of PublicWifi
	boolean ispublicWifiEnabled = false;
	try {

	    /**
	     * PRECONDITION 1 :Setting the Public wifi parameters and enabling the Public Wifi.
	     */
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRECONDITION 1: Setting the Public wifi parameters and enabling the Public Wifi.");
	    LOGGER.info("#####################################################################################");
	    isPublicWifiConfigurationSet = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS);
	    LOGGER.info(
		    "Setting the configurationparameters and enabling Public Wifi is-" + isPublicWifiConfigurationSet);
	    if (!isPublicWifiConfigurationSet) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "Public Wifi Enabling parameters are not configured");
	    }

	    /**
	     * PRECONDITION 2 :Checking Device.WiFi.SSID.10103.Enable is set to true
	     */

	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRECONDITION: Check and Enable 5GHz Public SSID 'Device.WiFi.SSID.10103.Enable' using WebPA.");
	    LOGGER.info("#####################################################################################");
	    if (!BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :NOT ABLE TO ENABLE THE 5GHZ PUBLIC SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }

	    /**
	     * STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10103.SSID to retrieve 5GHz Public SSID name by
	     * WebPA
	     */
	    tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10103.SSID to retrieve 5GHz Public SSID name by WebPA");
	    LOGGER.info("EXPECTED: should be able to get SSID Value by WebPA");
	    LOGGER.info("#####################################################################################");
	    String ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID);
	    LOGGER.info("5GHz Public SSID name retrieved using WebPa = " + ssidNameFromWebPa);
	    status = CommonMethods.isNotNull(ssidNameFromWebPa)
		    && ssidNameFromWebPa.equalsIgnoreCase(BroadBandTestConstants.PUBLIC_WIFI_SSID_5);
	    errorMessage = "Invalid/Incorrect 5GHz Public SSID name retrieved for Param 'Device.WiFi.SSID.10103.SSID' using WebPA command. Value retreived is:"
		    + ssidNameFromWebPa;
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully retrieved the 5GHz Public SSID name 'Device.WiFi.SSID.10103.SSID' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:Verify whether the system command provides the 5GHz Public SSID name and verify whether it is
	     * matching with WebPA response.
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verify whether the system command provides the 5GHz Public SSID name and verify whether it is matching with WebPA response.");
	    LOGGER.info(
		    "EXPECTED: should be able to get SSID Name inside the device and the output should match with webpa output");
	    LOGGER.info("#####################################################################################");
	    String interfaceName = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
		    BroadBandTestConstants.BAND_5GHZ, BroadBandTestConstants.PUBLIC_WIFI_TYPE,
		    BroadBandTestConstants.SSID_PARAM);
	    errorMessage = "Seems like Device command or Wi-Fi Interface used for retrieving 5GHz Public SSID name is empty or null";
	    if (CommonMethods.isNotNull(interfaceName)) {
		String ssidObtainedFromDevice = BroadBandWebPaUtils.getSsidNameRetrievedUsingDeviceCommand(device,
			tapEnv, interfaceName);
		LOGGER.info("5GHz Public SSID name retrieved using device command =" + ssidObtainedFromDevice);

		if (CommonMethods.isNotNull(ssidObtainedFromDevice)) {
		    status = ssidObtainedFromDevice.equalsIgnoreCase(ssidNameFromWebPa);
		    errorMessage = "5GHz Public SSID name using device command doesn't matches with name retrieved from WebPA command. ACTUAL : SSID name from device command = "
			    + ssidObtainedFromDevice
			    + " , SSID name 'Device.WiFi.SSID.10103.SSID' using WebPA command = " + ssidNameFromWebPa;
		} else {
		    status = false;
		    errorMessage = "Unable to retrieve the 5GHz Public SSID name using device command.";
		}
	    }
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully verified the 5GHz Public SSID name retrieved from WebPA with device command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} catch (Exception exception) {

	    status = false;
	    errorMessage = "Exception occurred while validating  5Ghz public SSID name 'Device.WiFi.SSID.10103.SSID' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}
	/**
	 * Disabling the Public Wifi 5GHZ on the device
	 */
	finally {
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("POST CONDITION : Disabling the Public Wifi 5GHZ by setting it to False");
	    LOGGER.info("#####################################################################################");
	    ispublicWifiEnabled = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.CONSTANT_3);
	    LOGGER.info("DISABLING THE PUBLIC WIFI 5GHZ ON THIS DEVICE-" + ispublicWifiEnabled);
	    if (!ispublicWifiEnabled) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "POST_CONDITION_FAILED :NOT ABLE TO DISABLE THE PUBLIC WIFI 5GHZ ON THIS DEVICE");
	    }
	}
    }

    /**
     * This test case checks the SSID on 2.4Ghz Private network by comparing the output from webpa with the output from
     * from device
     * <ol>
     * <li>STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10001.SSID to retrieve 2.4GHz Private SSID name by WebPA
     * </li>
     * 
     * <li>STEP 2:Verify whether the system command provides the 2.4GHz private SSID name and verify whether it is
     * matching with WebPA response.</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1014", testDecription = "Verification of 2.4Ghz Private network SSID name Device.WiFi.SSID.10001.SSID using WebPA")
    public void testValidateSsidNameFor2_4GhzPrivateNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-114";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;

	try {

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE_CONDITION: Check and Enable 2.4GHz Private SSID 'Device.WiFi.SSID.10001.Enable' using WebPA.");
	    LOGGER.info("#####################################################################################");
	    if (!BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :NOT ABLE TO ENABLE THE 2.4GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }

	    /**
	     * STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10001.SSID to retrieve 2.4GHz Private SSID name by
	     * WebPA
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10001.SSID to retrieve 2.4GHz Private SSID name by WebPA");
	    LOGGER.info("EXPECTED: should be able to get SSID Value by WebPA");
	    LOGGER.info("#####################################################################################");
	    String ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID);
	    LOGGER.info("2.4GHz private SSID name retrieved using WebPa = " + ssidNameFromWebPa);
	    status = CommonMethods.isNotNull(ssidNameFromWebPa);
	    errorMessage = "Unable to retrieve the 2.4GHz Private SSID name 'Device.WiFi.SSID.10001.SSID' using WebPA command.";
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully retrieved the 2.4GHz Private SSID name 'Device.WiFi.SSID.10001.SSID' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 2:Verify whether the system command provides the 2.4GHz private SSID name and matching with WebPA
	     * response.
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verify whether the system command provides the 2.4GHz private SSID name and verify whether it is matching with WebPA response.");
	    LOGGER.info(
		    "EXPECTED: should be able to get SSID Name inside the device and the output should match with webpa output");
	    LOGGER.info("#####################################################################################");
	    String interfaceName = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
		    BroadBandTestConstants.BAND_2_4GHZ, BroadBandTestConstants.PRIVATE_WIFI_TYPE,
		    BroadBandTestConstants.SSID_PARAM);
	    errorMessage = "Seems like Device command or Wi-Fi Interface used for retrieving 2.4GHz private SSID name is empty or null";
	    if (CommonMethods.isNotNull(interfaceName)) {
		String ssidObtainedFromDevice = BroadBandWebPaUtils.getSsidNameRetrievedUsingDeviceCommand(device,
			tapEnv, interfaceName);
		LOGGER.info("2.4GHz Private SSID name retrieved using device command =" + ssidObtainedFromDevice);

		if (CommonMethods.isNotNull(ssidObtainedFromDevice)) {
		    status = ssidObtainedFromDevice.contains(ssidNameFromWebPa);
		    errorMessage = "2.4GHz Private SSID name using device command doesn't matches with name retrieved from WebPA command. ACTUAL : SSID name from device command = "
			    + ssidObtainedFromDevice
			    + " , SSID name 'Device.WiFi.SSID.10001.SSID' using WebPA command = " + ssidNameFromWebPa;
		} else {
		    status = false;
		    errorMessage = "Unable to retrieve the 2.4GHz Private SSID name using device command.";
		}

	    }
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully verified the 2.4GHz private SSID name retrieved from WebPA with device command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} catch (Exception exception) {

	    status = false;
	    errorMessage = "Exception occurred while validating  2.4Ghz Private SSID name 'Device.WiFi.SSID.10001.SSID' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}
    }

    /**
     * This test case checks the SSID on 2.4Ghz Public network by comparing the output from webpa with the output from
     * the device
     * <ol>
     * <li>STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10003.SSID to retrieve 2.4GHz Public SSID name by WebPA
     * </li>
     * 
     * <li>STEP 2:Verify whether the system command provides the 2.4GHz Public SSID name and matching with WebPA
     * response.</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1016", testDecription = "Verification of 2.4Ghz Public network Device.WiFi.SSID.10003.SSID using WebPA")
    public void testValidateSsidNameFor2_4GhzPublicNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-116";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// stores the status of PublicWifi
	boolean isPublicWifiConfigurationSet = false;
	// stores the status of PublicWifi
	boolean ispublicWifiEnabled = false;
	try {
	    /**
	     * PRECONDITION 1 :Setting the configuration parameters and enabling Public Wifi 2.4 GHZ.
	     */
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRECONDITION 1: Setting the configuration parameters and enabling Public Wifi 2.4 GHZ.");
	    LOGGER.info("#####################################################################################");
	    isPublicWifiConfigurationSet = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ENABLE_STATUS);
	    LOGGER.info("Setting the configuration parameters and enabling Public Wifi 2.4 GHZ is-"
		    + isPublicWifiConfigurationSet);
	    if (!isPublicWifiConfigurationSet) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "Public Wifi  parameters for 2.4GHZ are not configured");
	    }

	    /**
	     * PRECONDITION 2 :Checking Device.WiFi.SSID.10003.Enable is set to true
	     */

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE_CONDITION: Check and Enable 2.4GHz Public SSID 'Device.WiFi.SSID.10003.Enable' using WebPA");
	    LOGGER.info("#####################################################################################");
	    if (!BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ENABLE_STATUS)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :NOT ABLE TO ENABLE THE 2.4GHZ PUBLIC SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }

	    /**
	     * STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10003.SSID to retrieve 2.4GHz Public SSID name by
	     * WebPA.
	     */
	    tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10003.SSID to retrieve 2.4GHz Public SSID name by WebPA");
	    LOGGER.info("EXPECTED: should be able to get SSID Value by WebPA");
	    LOGGER.info("#####################################################################################");
	    String ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID);
	    LOGGER.info("2.4GHz Public SSID name retrieved using WebPa =" + ssidNameFromWebPa);
	    status = CommonMethods.isNotNull(ssidNameFromWebPa)
		    && ssidNameFromWebPa.equalsIgnoreCase(BroadBandTestConstants.PUBLIC_WIFI_SSID_2);
	    errorMessage = "Invalid/Incorrect 2.4GHz Public SSID name retrieved for Param 'Device.WiFi.SSID.10003.SSID' using WebPA command. Value retried is:"
		    + ssidNameFromWebPa;

	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully retrieved the 2.4GHz Public SSID name 'Device.WiFi.SSID.10003.SSID' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 2:Verify whether the system command provides the 2.4GHz Public SSID name and matching with WebPA
	     * response.
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verify whether the system command provides the 2.4GHz Public  SSID name and matching with WebPA response");
	    LOGGER.info(
		    "EXPECTED: should be able to get SSID Name inside the device and the output should match with webpa output");
	    LOGGER.info("#####################################################################################");
	    String interfaceName = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
		    BroadBandTestConstants.BAND_2_4GHZ, BroadBandTestConstants.PUBLIC_WIFI_TYPE,
		    BroadBandTestConstants.SSID_PARAM);
	    errorMessage = "Seems like Device command or Wi-Fi Interface used for retrieving 2.4GHz Public SSID name is empty or null";
	    if (CommonMethods.isNotNull(interfaceName)) {
		String ssidObtainedFromDevice = BroadBandWebPaUtils.getSsidNameRetrievedUsingDeviceCommand(device,
			tapEnv, interfaceName);
		LOGGER.info("2.4GHz Public SSID name retrieved using device command =" + ssidObtainedFromDevice);

		if (CommonMethods.isNotNull(ssidObtainedFromDevice)) {
		    status = ssidObtainedFromDevice.equalsIgnoreCase(ssidNameFromWebPa);
		    errorMessage = "2.4GHz Public SSID name using device command doesn't matches with name retrieved from WebPA command. ACTUAL : SSID name from device command = "
			    + ssidObtainedFromDevice
			    + " , SSID name 'Device.WiFi.SSID.10003.SSID' using WebPA command = " + ssidNameFromWebPa;
		} else {
		    status = false;
		    errorMessage = "Unable to retrieve the 2.4GHz Public SSID name using device command.";
		}
	    }
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully verified the 2.4GHz Public SSID name retrieved from WebPA with device command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} catch (Exception exception) {

	    status = false;
	    errorMessage = "Exception occurred while validating  2.4Ghz public SSID name 'Device.WiFi.SSID.10003.SSID' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}
	/**
	 * Disabling the Public Wifi 2.4GHZ on the device
	 */
	finally {
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("POST CONDITION : Disabling the Public Wifi 2.4GHZ by setting it to False");
	    LOGGER.info("#####################################################################################");
	    ispublicWifiEnabled = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.CONSTANT_3);
	    LOGGER.info("DISABLING THE PUBLIC WIFI ON THIS DEVICE-" + ispublicWifiEnabled);
	    if (!ispublicWifiEnabled) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "POST_CONDITION_FAILED :NOT ABLE TO DISABLE THE PUBLIC WIFI 2.4GHZ ON THIS DEVICE");
	    }
	}

    }

    /**
     * This test case checks the MAC Address on 5Ghz Private network by comparing the output from webpa with the output
     * from device
     * 
     * <ol>
     * <li>STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10101.MACAddress to retrieve 5GHz Private MACAddress by
     * WebPA</li>
     * 
     * <li>STEP 2:Verify whether the system command provides the 5Ghz Private MAC Address and matching with WebPA
     * response.</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1017", testDecription = "Verification of 5Ghz Private network MacAddress Device.WiFi.SSID.10101.MACAddress using WebPA")
    public void testValidateMacAddresssFor5GhzPrivateNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-117";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;

	try {
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE_CONDITION: Check and Enable 5GHz Private SSID 'Device.WiFi.SSID.10101.Enable' using WebPA");
	    LOGGER.info("#####################################################################################");
	    if (!BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :NOT ABLE TO ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }
	    /**
	     * STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10101.MACAddress to retrieve 5GHz Private MACAddress
	     * by WebPA
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10101.MACAddress to retrieve 5GHz Private MACAddress by WebPA");
	    LOGGER.info("EXPECTED: should be able to get MAC Address Value by WebPA");
	    LOGGER.info("#####################################################################################");
	    String macAddressFromWebPa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_MAC_ADDRESS);
	    LOGGER.info("5GHz Private MacAddress retrieved using WebPa =" + macAddressFromWebPa);
	    status = CommonMethods.isNotNull(macAddressFromWebPa);
	    errorMessage = "Unable to retrieve the 5GHz Public Mac Address 'Device.WiFi.SSID.10101.MACAddress' using WebPA command.";
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully retrieved the 5GHz Private MacAddress 'Device.WiFi.SSID.10101.MACAddress' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    //
	    /**
	     * STEP 2:Verify whether the system command provides the 5Ghz Private MAC Address and matching with WebPA
	     * response.
	     */

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verify whether the system command provides the 5Ghz Private  MAC Address and matching with WebPA response");
	    LOGGER.info(
		    "EXPECTED: should be able to get MAC Address from inside the device, and the output should match with webpa output");
	    LOGGER.info("#####################################################################################");
	    String interfaceName = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
		    BroadBandTestConstants.BAND_5GHZ, BroadBandTestConstants.PRIVATE_WIFI_TYPE,
		    BroadBandTestConstants.BSSID_PARAM);
	    errorMessage = "Seems like Device command or Wi-Fi Interface used for retrieving 5GHz private Mac Address is empty or null";
	    if (CommonMethods.isNotNull(interfaceName)) {
		String macAddressObtainedFromDevice = BroadBandWebPaUtils
			.getMacAddressRetrievedUsingDeviceCommand(device, tapEnv, interfaceName);
		LOGGER.info("5GHz Private MACAddress retrieved using device command =" + macAddressObtainedFromDevice);

		if (CommonMethods.isNotNull(macAddressObtainedFromDevice)) {
		    status = macAddressObtainedFromDevice.contains(macAddressFromWebPa);
		    errorMessage = "5GHz Private MACAddress using device command doesn't matches with name retrieved from WebPA command. ACTUAL : MAC Address from device command = "
			    + macAddressObtainedFromDevice
			    + " , MACAddress from 'Device.WiFi.SSID.10101.MACAddress' using WebPA command = "
			    + macAddressFromWebPa;
		} else {
		    status = false;
		    errorMessage = "Unable to retrieve the 5GHz Private MACAddress using device command.";
		}

	    }
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully verified the 5GHz private MACAddress retrieved from WebPA with device command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while validating  5Ghz Private MACAddress 'Device.WiFi.SSID.10101.MACAddress' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}
    }

    /**
     * This test case checks the MAC Address on 5Ghz Public network by comparing the output from webpa with the output
     * from the device
     * <ol>
     * <li>STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10103.MACAddress to retrieve 5GHz Public MACAddress by
     * WebPA</li>
     * 
     * <li>STEP 2:Verify whether the system command provides the 5Ghz Public wifi MAC Address and matching with WebPA
     * response.</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1019", testDecription = "Verification of 5Ghz Public network MacAddress Device.WiFi.SSID.10103.MACAddress using WebPA")
    public void testValidateMacAddresssFor5GhzPublicNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-119";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// stores the status of PublicWifi
	boolean isPublicWifiConfigurationSet = false;
	// stores the status of PublicWifi
	boolean ispublicWifiEnabled = false;
	try {

	    /**
	     * PRECONDITION 1 :Setting the parameters and enabling Public Wifi 5GHZ.
	     */
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRECONDITION 1: Setting the configuration parameters and enabling Public Wifi  5GHZ.");
	    LOGGER.info("#####################################################################################");
	    isPublicWifiConfigurationSet = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS);
	    LOGGER.info("Setting the configuration parameters and enabling Public Wifi 5GHZ is-"
		    + isPublicWifiConfigurationSet);
	    if (!isPublicWifiConfigurationSet) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "Public Wifi 5GHZ Enabling parameters are not configured");
	    }

	    /**
	     * PRECONDITION 2 :Checking Device.WiFi.SSID.10103.Enable is set to true
	     */

	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE_CONDITION: Check and Enable 5GHz Public SSID 'Device.WiFi.SSID.10103.Enable' using WebPA");
	    LOGGER.info("#####################################################################################");
	    if (!BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :NOT ABLE TO ENABLE THE 5GHZ PUBLIC SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }

	    /**
	     * STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10103.MACAddress to retrieve 5GHz Public MACAddress
	     * by WebPA
	     */
	    tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10103.MACAddress to retrieve 5GHz Public MACAddress by WebPA");
	    LOGGER.info("EXPECTED: should be able to get MAC Address Value by WebPA");
	    LOGGER.info("#####################################################################################");
	    String macAddressFromWebPa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_MAC_ADDRESS);
	    LOGGER.info("5GHz Public Wifi MacAddress retrieved using WebPa =" + macAddressFromWebPa);
	    status = CommonMethods.isNotNull(macAddressFromWebPa);
	    errorMessage = "Unable to retrieve the 5GHz Public Mac Address 'Device.WiFi.SSID.10103.MACAddress' using WebPA command.";

	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully retrieved the 5GHz Public MacAddress 'Device.WiFi.SSID.10103.MACAddress' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 2:Verify whether the system command provides the 5Ghz Public wifi MAC Address and matching with
	     * WebPA response
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verify whether the system command provides the 5Ghz Public wifi   MAC Address and matching with WebPA response.");
	    LOGGER.info(
		    "EXPECTED: should be able to get MAC Address from inside the device, and the output should match with webpa output");
	    LOGGER.info("#####################################################################################");
	    String interfaceName = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
		    BroadBandTestConstants.BAND_5GHZ, BroadBandTestConstants.PUBLIC_WIFI_TYPE,
		    BroadBandTestConstants.BSSID_PARAM);
	    errorMessage = "Seems like Device command or Wi-Fi Interface used for retrieving 5GHz Public MacAddress is empty or null";
	    if (CommonMethods.isNotNull(interfaceName)) {
		String macAddressObtainedFromDevice = BroadBandWebPaUtils
			.getMacAddressRetrievedUsingDeviceCommand(device, tapEnv, interfaceName);
		LOGGER.info(
			"5GHz Public Wifi MACAddress retrieved using device command =" + macAddressObtainedFromDevice);
		if (CommonMethods.isNotNull(macAddressObtainedFromDevice)) {
		    status = macAddressObtainedFromDevice.contains(macAddressFromWebPa);
		    errorMessage = "5GHz Public Wifi MACAddress using device command doesn't matches with name retrieved from WebPA command. ACTUAL : MAC Address from device command = "
			    + macAddressObtainedFromDevice
			    + " , MACAddress from 'Device.WiFi.SSID.10103.MACAddress' using WebPA command = "
			    + macAddressFromWebPa;
		} else {
		    status = false;
		    errorMessage = "Unable to retrieve the 5GHz Public MACAddress using device command.";
		}
	    }
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully verified the 5GHz Public MACAddress retrieved from WebPA with device command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while validating  5Ghz Public MACAddress 'Device.WiFi.SSID.10103.MACAddress' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}
	/**
	 * Disabling the Public Wifi 5GHZ on the device
	 */
	finally {
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("POST CONDITION : Disabling the Public Wifi 5GHZ by setting it to False");
	    LOGGER.info("#####################################################################################");
	    ispublicWifiEnabled = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.CONSTANT_3);
	    LOGGER.info("DISABLING THE PUBLIC WIFI 5GHZ ON THIS DEVICE-" + ispublicWifiEnabled);
	    if (!ispublicWifiEnabled) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "POST_CONDITION_FAILED :NOT ABLE TO DISABLE THE PUBLIC WIFI 5GHZ ON THIS DEVICE");
	    }
	}

    }

    /**
     * This test case checks the MAC Address on 2.4Ghz Private network by comparing the output from webpa with the
     * output from the device *
     * <ol>
     * <li>STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10001.MACAddress to retrieve 2.4GHz Private MACAddress
     * by WebPA</li>
     * 
     * <li>STEP 2:Verify whether the system command provides the 2.4Ghz Private MAC Address and matching with WebPA
     * response.</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1020", testDecription = "Verification of 2.4Ghz Private network MacAddress Device.WiFi.SSID.10001.MACAddress using WebPA")
    public void testValidateMacAddresssFor2_4GhzPrivateNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-120";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	try {
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE_CONDITION: Check and Enable 2.4GHz Private SSID 'Device.WiFi.SSID.10001.Enable' using WebPA");
	    LOGGER.info("#####################################################################################");
	    if (!BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :NOT ABLE TO ENABLE THE 2.4GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }
	    /**
	     * STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10001.MACAddress to retrieve 2.4GHz Private
	     * MACAddress by WebPA.
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10001.MACAddress to retrieve 2.4GHz Private MACAddress by WebPA");
	    LOGGER.info("EXPECTED: should be able to get MAC Address Value");
	    LOGGER.info("#####################################################################################");
	    String macAddressFromWebPa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_MAC_ADDRESS);
	    LOGGER.info("2.4GHz Private MacAddress retrieved using WebPa =" + macAddressFromWebPa);
	    status = CommonMethods.isNotNull(macAddressFromWebPa);
	    errorMessage = "Unable to retrieve the 2.4 GHz private Mac Address 'Device.WiFi.SSID.10001.MACAddress' using WebPA command.";
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully retrieved the 2.4GHz Private MacAddress 'Device.WiFi.SSID.10001.MACAddress' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 2:Verify whether the system command provides the 2.4Ghz Private MAC Address and matching with WebPA
	     * response.
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verify whether the system command provides the 2.4Ghz Private  MAC Address and matching with WebPA response.");
	    LOGGER.info(
		    "EXPECTED: should be able to get MAC Address from inside the device, and the output should match with webpa output");
	    LOGGER.info("#####################################################################################");
	    String interfaceName = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
		    BroadBandTestConstants.BAND_2_4GHZ, BroadBandTestConstants.PRIVATE_WIFI_TYPE,
		    BroadBandTestConstants.BSSID_PARAM);
	    errorMessage = "Seems like Device command or Wi-Fi Interface used for retrieving 2.4GHz private MacAddress is empty or null";
	    if (CommonMethods.isNotNull(interfaceName)) {
		String macAddressObtainedFromDevice = BroadBandWebPaUtils
			.getMacAddressRetrievedUsingDeviceCommand(device, tapEnv, interfaceName);
		LOGGER.info(
			"2.4GHz Private MACAddress retrieved using device command =" + macAddressObtainedFromDevice);
		if (CommonMethods.isNotNull(macAddressObtainedFromDevice)) {
		    status = macAddressObtainedFromDevice.contains(macAddressFromWebPa);
		    errorMessage = "2.4GHz Private MACAddress using device command doesn't matches with name retrieved from WebPA command. ACTUAL : MAC Address from device command = "
			    + macAddressObtainedFromDevice
			    + " , MACAddress from 'Device.WiFi.SSID.10001.MACAddress' using WebPA command = "
			    + macAddressFromWebPa;
		} else {
		    status = false;
		    errorMessage = "Unable to retrieve the 2.4 Private MACAddress using device command.";
		}
	    }
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully verified the 2.4GHz Private MACAddress retrieved from WebPA with device command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while validating  2.4Ghz Private MACAddress 'Device.WiFi.SSID.10001.MACAddress' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}
    }

    /**
     * This test case checks the MAC Address on 2.4Ghz Public network by comparing the output from webpa with the output
     * from the device
     * <ol>
     * <li>STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10003.MACAddress to retrieve 2.4GHz Public MACAddress by
     * WebPA</li>
     * 
     * <li>STEP 2:Verify whether the system command provides the 2.4Ghz Public wifi MAC Address and matching with WebPA
     * response.</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1022", testDecription = "Verification of 2.4Ghz Public network MACAddress Device.WiFi.SSID.10003.MacAddress using WebPA")
    public void testValidateMacAddresssFor2_4GhzPublicNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-122";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// stores the status of PublicWifi
	boolean isPublicWifiConfigurationSet = false;
	// stores the status of PublicWifi
	boolean ispublicWifiEnabled = false;
	try {
	    /**
	     * PRECONDITION 1 :Setting the configuration parameters and enabling Public Wifi 2.4GHZ.
	     */
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRECONDITION 1: Setting the configuration parameters and enabling Public Wifi 2.4GHZ.");
	    LOGGER.info("#####################################################################################");
	    isPublicWifiConfigurationSet = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ENABLE_STATUS);
	    LOGGER.info("Setting the parameters and enabling Public Wifi 2.4 GHZ is-" + isPublicWifiConfigurationSet);
	    if (!isPublicWifiConfigurationSet) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "Public Wifi Enabling parameters for 2.4GHZ are not configured");
	    }

	    /**
	     * PRECONDITION 2 :Checking Device.WiFi.SSID.10003.Enable is set to true
	     */

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE_CONDITION: Check and Enable 2.4GHz Public SSID 'Device.WiFi.SSID.10003.Enable' using WebPA");
	    LOGGER.info("#####################################################################################");
	    if (!BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ENABLE_STATUS)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :NOT ABLE TO ENABLE THE 2.4GHZ PUBLIC SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }

	    /**
	     * STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10003.MACAddress to retrieve 2.4GHz Public
	     * MACAddress by WebPA
	     */
	    tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Execute the TR-181 parameter-Device.WiFi.SSID.10003.MACAddress to retrieve 2.4GHz Public MACAddress by WebPA");
	    LOGGER.info("EXPECTED: should be able to get MAC Address Value");
	    LOGGER.info("#####################################################################################");
	    String macAddressFromWebPa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_MAC_ADDRESS);
	    LOGGER.info("2.4GHz Public MacAddress retrieved using WebPa =" + macAddressFromWebPa);
	    status = CommonMethods.isNotNull(macAddressFromWebPa);
	    errorMessage = "Successfully retrieved the 2.4GHz Public MacAddress 'Device.WiFi.SSID.10003.MACAddress' using WebPA command.";
	    LOGGER.info("S1 ACTUAL : "
		    + (status ? "webPA Get on Device.WiFi.SSID.10003.MACAddress - SUCCESS" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 2:Verify whether the system command provides the 2.4Ghz Public wifi MAC Address and matching with
	     * WebPA response.
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verify whether the system command provides the 2.4Ghz Public wifi   MAC Address and matching with WebPA response.");
	    LOGGER.info(
		    "EXPECTED: should be able to get MAC Address from inside the device, and the output should match with webpa output");
	    LOGGER.info("#####################################################################################");
	    String interfaceName = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
		    BroadBandTestConstants.BAND_2_4GHZ, BroadBandTestConstants.PUBLIC_WIFI_TYPE,
		    BroadBandTestConstants.BSSID_PARAM);
	    errorMessage = "Seems like Device command or Wi-Fi Interface used for retrieving 2.4GHz Public MacAddress is empty or null";
	    if (CommonMethods.isNotNull(interfaceName)) {
		String macAddressObtainedFromDevice = BroadBandWebPaUtils
			.getMacAddressRetrievedUsingDeviceCommand(device, tapEnv, interfaceName);
		LOGGER.info("2.4GHz Public Wifi MACAddress retrieved using device command ="
			+ macAddressObtainedFromDevice);
		if (CommonMethods.isNotNull(macAddressObtainedFromDevice)) {
		    status = macAddressObtainedFromDevice.contains(macAddressFromWebPa);
		    errorMessage = "2.4GHz Public MACAddress using device command doesn't matches with name retrieved from WebPA command. ACTUAL : MAC Address from device command = "
			    + macAddressObtainedFromDevice
			    + " , MACAddress from 'Device.WiFi.SSID.10003.MACAddress' using WebPA command = "
			    + macAddressFromWebPa;
		} else {
		    status = false;
		    errorMessage = "Unable to retrieve the 2.4GHZ Public MACAddress using device command.";
		}
	    }
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully verified the 2.4GHz Public MACAddress retrieved from WebPA with device command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} catch (Exception exception) {
	    errorMessage = "Exception occurred while validating  2.4Ghz Public MACAddress 'Device.WiFi.SSID.10003.MACAddress' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}
	/**
	 * Disabling the Public Wifi 2.4GHZ on the device
	 */
	finally {
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("POST CONDITION : Disabling the Public Wifi 2.4GHZ by setting it to False");
	    LOGGER.info("#####################################################################################");
	    ispublicWifiEnabled = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.CONSTANT_3);
	    LOGGER.info("DISABLING THE PUBLIC WIFI ON THIS DEVICE-" + ispublicWifiEnabled);
	    if (!ispublicWifiEnabled) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "POST_CONDITION_FAILED :NOT ABLE TO DISABLE THE PUBLIC WIFI 2.4GHZ ON THIS DEVICE");
	    }
	}
    }

    /**
     * This test case checks the validation of SSID Enable/Disble from device side for 5Ghz Private interface
     * 
     * <ol>
     * <li>STEP 1:Disable the SSID of 5Ghz Private Interface and verify the same from device side</li>
     * 
     * <li>STEP 2:Enable the SSID of 5Ghz Private Interface and verify the same from device side</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1023", testDecription = "Verifying Device.WiFi.SSID.10101.Enable using WebPA")
    public void testValidateSsidEnabledOn5GhzPrivateInterface(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-123";
	// String to store the test step number
	String testStepNumber = null;
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	try {
	    /**
	     * STEP 1:Disable the SSID of 5Ghz Private Interface and verify the same from device side
	     */
	    testStepNumber = "s1";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 1:Disable the SSID of 5Ghz Private  Interface and verify the same from device side");
	    LOGGER.info(
		    "EXPECTED: should be able to set the parameter with Value false and the same should be relected in the device");
	    LOGGER.info("#####################################################################################");
	    String interfaceName = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
		    BroadBandTestConstants.BAND_5GHZ, BroadBandTestConstants.PRIVATE_WIFI_TYPE,
		    BroadBandTestConstants.BSSID_PARAM);
	    errorMessage = "Seems like Device command or Wi-Fi Interface used for retrieving 5GHz private bSSID name is empty or null";
	    if (CommonMethods.isNotNull(interfaceName)) {
		status = BroadBandWebPaUtils.disableSsidAndVerifyFromDevice(device, tapEnv, interfaceName,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS);
		errorMessage = "Unable to set the parameter Device.WiFi.SSID.10101.Enable with value false";
	    }
	    LOGGER.info("S1 ACTUAL : "
		    + (status ? "Device.WiFi.SSID.10101.Enable is set to false and the same is verified in the device"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 2:Enable the SSID of 5Ghz Private Interface and verify the same from device side
	     */

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 2:Enable the SSID of 5Ghz Private  Interface and verify the same from device side");
	    LOGGER.info(
		    "EXPECTED: should be able to set the parameter with Value True and the same should be relected in the device");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.enableSsidAndVerifyFromDevice(device, tapEnv, interfaceName,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS);
	    errorMessage = "Unable to set the parameter Device.WiFi.SSID.10101.Enable with value True and verify the same";
	    LOGGER.info("S2 ACTUAL : "
		    + (status ? "Device.WiFi.SSID.10101.Enable is set to True and the same is verified in the device"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while validating  5Ghz Private Interface 'Device.WiFi.SSID.10101.Enable' using WebPA : "
		    + exception.getMessage();
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	}
    }

    /**
     * This test case checks the validation of SSID Enable/Disble from device side for 5Ghz Public interface
     * 
     * <ol>
     * <li>STEP 1:Enable the SSID of 5Ghz Public Interface and verify the same from device side</li>
     * 
     * <li>STEP 2:Disable the SSID of 5Ghz Public Interface and verify the same from device side</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1025", testDecription = "Verifying Device.WiFi.SSID.10103.Enable using WebPA")
    public void testValidateSsidEnabledOn5GhzPublicInterface(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-125";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// stores the status of PublicWifi
	boolean publicWifiEnable = false;
	try {

	    /**
	     * PRECONDITION :Setting the Wifi parameters for enabling Public Wifi.
	     */
	    LOGGER.info("PRECONDITION: Setting the Wifi parameters for enabling Public Wifi");
	    publicWifiEnable = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS);
	    LOGGER.info("Public wifi enabling is  " + publicWifiEnable);
	    if (!publicWifiEnable) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "Public Wifi is not enabled");
	    }

	    /**
	     * STEP 1:Enable the SSID of 5Ghz Public Interface and verify the same from device side
	     */
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 1:Enable the SSID of 5Ghz Public Interface and verify the same from device side");
	    LOGGER.info(
		    "EXPECTED: should be able to set the parameter with Value true and the same should be relected in the device");
	    LOGGER.info("#####################################################################################");
	    String interfaceName = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
		    BroadBandTestConstants.BAND_5GHZ, BroadBandTestConstants.PUBLIC_WIFI_TYPE,
		    BroadBandTestConstants.BSSID_PARAM);
	    errorMessage = "Seems like Device command or Wi-Fi Interface used for retrieving 5GHz Public BSSID name is empty or null";
	    if (CommonMethods.isNotNull(interfaceName)) {
		status = BroadBandWebPaUtils.enableSsidAndVerifyFromDevice(device, tapEnv, interfaceName,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS);
		errorMessage = "unable to  set the parameter Device.WiFi.SSID.10103.Enable with value true";
	    }
	    LOGGER.info("S1 ACTUAL : "
		    + (status ? "Device.WiFi.SSID.10103.Enable is set to true and the same is verified in the device"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 2:Disable the SSID of 5Ghz Public Interface and verify the same from device side
	     */

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 2:Disable the SSID of 5Ghz Public  Interface and verify the same from device side");
	    LOGGER.info(
		    "EXPECTED: should be able to set the parameter with Value false and the same should be relected in the device");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandWebPaUtils.disableSsidAndVerifyFromDevice(device, tapEnv, interfaceName,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI);
	    errorMessage = "Unable to  set the parameter Device.WiFi.SSID.10103.Enable with value false and verify the same";
	    LOGGER.info("S2 ACTUAL : "
		    + (status ? "Device.WiFi.SSID.10103.Enable is set to false and the same is verified in the device"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while validating  5Ghz Public Interface 'Device.WiFi.SSID.10103.Enable' using WebPA : "
		    + exception.getMessage();
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	}

    }

    /**
     * This test case checks the SSIDAdvertisementEnabled on 2.4Ghz Private network by WebPA and verifies by SNMP
     * 
     * <ol>
     * <li>STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled by setting value as
     * false and verify the assigned value using WebPA get command</li>
     * 
     * <li>STEP 2:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10001) and cross checked that
     * we are getting actual value '1' for false.</li>
     * 
     * <li>STEP 3:Enable the TR181 parameter Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled by setting value as
     * true and verify the assigned value using WebPA get command</li>
     * 
     * <li>STEP 4:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10001) and cross checked that
     * we are getting actual value '2' for true.
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1030", testDecription = "Verification of 2.4Ghz Private wifi been broadcasted by using this parameter 'Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled' using WebPA")
    public void testValidateSSIDAdvertisementEnabledFor2_4GhzPrivateNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-130";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// Stores SNMP output
	String snmpOutput = null;
	// stores the Access point status
	boolean isAccessPointEnabled = false;
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION :DESCRIPTION :Check and Enable 2.4GHz Private SSID 'Device.WiFi.SSID.10001.Enable' using WebPA.");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION : ACTION : Check and Enable 2.4GHz Private SSID");
	    LOGGER.info("PRE-CONDITION : EXPECTED :  2.4GHz Private SSID should be enabled successfully");

	    if (!BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :NOT ABLE TO ENABLE THE 2.4GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-XB-WIFI-WEBPA-1030");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verification of 2.4Ghz Private wifi been broadcasted by using this parameter 'Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled' using WebPA.");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled by setting
	     * value as false and verify the assigned value using WebPA get command
	     */
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled by setting value as false and verify the assigned value using WebPA get command");
	    LOGGER.info(
		    "EXPECTED: TR181 parameter:Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled should be disabled with Success Message and Staus value should be 'false'");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
	    errorMessage = "Unable to disable the TR-181 parameter 'Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled' by setting value as false";
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully disabled the TR-181 parameter 'Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled' by setting value as false"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10001) and cross verify
	     * that we are getting actual value '1' for false.
	     * 
	     */

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10001) and cross verify that we are getting actual value as '1' for false.");
	    LOGGER.info("EXPECTED: should be able to retrieve the value using SNMP command");
	    LOGGER.info("#####################################################################################");
	    if (!CommonMethods.isRunningEthwanMode()) {
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		snmpOutput = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_PRIVATE_2_4GHZ_SSID_ADVERTISEMENT_ENABLED.getOid(),
			BroadBandSnmpMib.ECM_PRIVATE_2_4GHZ_SSID_ADVERTISEMENT_ENABLED.getTableIndex());
		status = CommonMethods.isNotNull(snmpOutput)
			&& snmpOutput.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
		errorMessage = "unable to retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10001) and cross checked  that we are getting actual value as '1' for false.";
		LOGGER.info("S2 ACTUAL : " + (status
			? "Successfully verified  the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10001) and cross checked  that we are getting actual value '1' for false."
			: errorMessage));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	    } else {

		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			"This test step not applicable for Ethwan Enable Devices", true);

	    }

	    /**
	     * STEP 3:Enable the TR181 parameter Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled by setting value
	     * as true and verify the assigned value using WebPA get command
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3:Enable the TR181 parameter Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled by setting value as true and verify the assigned value using WebPA get command");
	    LOGGER.info(
		    "EXPECTED: TR181 parameter:Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled should be disabled with Success Message and Staus value should be 'false'");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	    if (status) {
		isAccessPointEnabled = true;
	    }
	    errorMessage = "Unable to enable the TR-181 parameter 'Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled' by setting value as true";
	    LOGGER.info("S3 ACTUAL : " + (status
		    ? "Successfully enabled the TR-181 parameter 'Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled' by setting value as true"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 4:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10001) and cross verify
	     * that we are getting actual value '2' for true.
	     * 
	     */

	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 4:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10001) and cross verify that we are getting actual value as '2' for true.");
	    LOGGER.info("EXPECTED: should be able to retrieve the value using SNMP command");
	    LOGGER.info("#####################################################################################");
	    if (!CommonMethods.isRunningEthwanMode()) {
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		snmpOutput = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_PRIVATE_2_4GHZ_SSID_ADVERTISEMENT_ENABLED.getOid(),
			BroadBandSnmpMib.ECM_PRIVATE_2_4GHZ_SSID_ADVERTISEMENT_ENABLED.getTableIndex());

		status = CommonMethods.isNotNull(snmpOutput)
			&& snmpOutput.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO);
		errorMessage = "unable to retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10001)  and cross verify that we are getting actual value '2' for true.";
		LOGGER.info("S4 ACTUAL : " + (status
			? "Successfully verified  the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10001) and cross verify that we are getting actual value '2' for true."
			: errorMessage));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    } else {

		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			"This test step not applicable for Ethwan Enable Devices", true);

	    }

	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while verifying the  2.4Ghz Private 'Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	} finally {
	    if (!isAccessPointEnabled) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		LOGGER.info("#####################################################################################");
		LOGGER.info(
			"POST-CONDITION : DESCRIPTION : Reverting back to the original state  by setting the value as 'true' for the TR181 parameter 'Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled'");
		LOGGER.info(
			"POST-CONDITION : ACTION : TR181 parameter 'Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled' should be set as true");
		LOGGER.info(
			"POST-CONDITION :EXPECTED: Should be able to set as true to the TR181 parameter 'Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled' successfully ");
		LOGGER.info("#####################################################################################");
		boolean result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
		LOGGER.info("### POST-CONDITION ### END POST-CONFIGURATIONS ON THE DEVICE: " + result);
	    }
	}
    }

    /**
     * This test case checks the SSIDAdvertisementEnabled on 5Ghz Private network by WebPA and verifies by SNMP
     * 
     * <ol>
     * <li>STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled by setting value as
     * false and verify the assigned value using WebPA get command</li>
     * 
     * <li>STEP 2:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10101) and cross verify that we
     * are getting actual value '1' for false.</li>
     * 
     * <li>STEP 3:Enable the TR181 parameter Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled by setting value as
     * true and verify the assigned value using WebPA get command</li>
     * 
     * <li>STEP 4:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10101) and cross checked that
     * we are getting actual value '2' for true.
     * </ol>
     * 
     * @param device
     *            Dut to be used
     * @Refactor Alan_Bivera
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1031", testDecription = "Verification of 5Ghz Private wifi been broadcasted by using this parameter 'Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled' using WebPA")
    public void testValidateSSIDAdvertisementEnabledFor5GhzPrivateNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-131";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// Stores SNMP output
	String snmpOutput = null;
	// stores the Access point status
	boolean isAccessPointEnabled = false;
	try {
	    //
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION :DESCRIPTION :Check and Enable 5GHz Private SSID 'Device.WiFi.SSID.10101.Enable' using WebPA.");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION : ACTION : Check and Enable 5GHz Private SSID");
	    LOGGER.info("PRE-CONDITION : EXPECTED :  5GHz Private SSID should be enabled successfully");

	    if (!BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :NOT ABLE TO ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-WEBPA-1031");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verification of 5Ghz Private wifi been broadcasted by using this parameter 'Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled' using WebPA.");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled by setting
	     * value as false and verify the assigned value using WebPA get command
	     */
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled by setting value as false and verify the assigned value using WebPA get command");
	    LOGGER.info(
		    "EXPECTED: TR181 parameter:Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled should be disabled with Success Message and Staus value should be 'false'");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
	    errorMessage = "Unable to disable the TR-181 parameter 'Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled' by setting value as false";
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully disabled the TR-181 parameter 'Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled' by setting value as false"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10101) and cross verify
	     * that we are getting actual value '1' for false.
	     * 
	     */

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10101) and cross verify that we are getting actual value as '1' for false.");
	    LOGGER.info("EXPECTED: should be able to retrieve the value using SNMP command");
	    LOGGER.info("#####################################################################################");
	    if (!CommonMethods.isRunningEthwanMode()) {
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		snmpOutput = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_PRIVATE_5GHZ_SSID_ADVERTISEMENT_ENABLED.getOid(),
			BroadBandSnmpMib.ECM_PRIVATE_5GHZ_SSID_ADVERTISEMENT_ENABLED.getTableIndex());

		status = CommonMethods.isNotNull(snmpOutput)
			&& snmpOutput.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
		errorMessage = "unable to retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10101) and cross checked  that we are getting actual value '1' for false.";
		LOGGER.info("S2 ACTUAL : " + (status
			? "Successfully verified  the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10101) and cross checked  that we are getting actual value '1' for false."
			: errorMessage));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	    } else {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			"This test step not applicable for Ethwan Enable Mode Devices", false);
	    }

	    /**
	     * STEP 3:Enable the TR181 parameter Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled by setting value
	     * as true and verify the assigned value using WebPA get command
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3:Enable the TR181 parameter Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled by setting value as true and verify the assigned value using WebPA get command");
	    LOGGER.info(
		    "EXPECTED: TR181 parameter:Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled should be disabled with Success Message and Staus value should be 'false'");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	    if (status) {
		isAccessPointEnabled = true;
	    }
	    errorMessage = "Unable to enable the TR-181 parameter 'Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled' by setting value as true";
	    LOGGER.info("S3 ACTUAL : " + (status
		    ? "Successfully enabled the TR-181 parameter 'Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled' by setting value as true"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 4:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10101) and cross verify
	     * that we are getting actual value '2' for true.
	     * 
	     */

	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 4:Retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10101) and cross verify that we are getting actual value as '2' for true.");
	    LOGGER.info("EXPECTED: should be able to retrieve the value using SNMP command");
	    LOGGER.info("#####################################################################################");
	    if (!CommonMethods.isRunningEthwanMode()) {
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		snmpOutput = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_PRIVATE_5GHZ_SSID_ADVERTISEMENT_ENABLED.getOid(),
			BroadBandSnmpMib.ECM_PRIVATE_5GHZ_SSID_ADVERTISEMENT_ENABLED.getTableIndex());

		status = CommonMethods.isNotNull(snmpOutput)
			&& snmpOutput.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO);
		errorMessage = "unable to retrieve the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10101) and cross verify that we are getting actual value '2' for true.";
		LOGGER.info("S4 ACTUAL : " + (status
			? "Successfully verified  the value using SNMP command(1.3.6.1.4.1.17270.50.2.2.2.1.1.5.10101) and cross verify that we are getting actual value '2' for true."
			: errorMessage));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    } else {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			"This test step not applicable for  Ethwan Enable Mode Devices", false);
	    }

	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while verifying the  5Ghz Private 'Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	} finally {
	    if (!isAccessPointEnabled) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		LOGGER.info("#####################################################################################");
		LOGGER.info(
			"POST-CONDITION : DESCRIPTION : Reverting back to the original state  by setting the value as 'true' for the TR181 parameter 'Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled'");
		LOGGER.info(
			"POST-CONDITION : ACTION : TR181 parameter 'Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled' should be set as true");
		LOGGER.info(
			"POST-CONDITION :EXPECTED: Should be able to set as true to the TR181 parameter 'Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled' successfully ");
		LOGGER.info("#####################################################################################");
		boolean result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
		LOGGER.info("### POST-CONDITION  ### END POST-CONFIGURATIONS ON THE DEVICE: " + result);
	    }
	}
    }

    /**
     * This test case checks the SSIDAdvertisementEnabled on 2.4Ghz Public wifi network by WebPA and verifies by SNMP
     * 
     * <ol>
     * <li>STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled by setting value as
     * false and verify the assigned value using WebPA get command</li>
     * <li>STEP 2:Enable the TR181 parameter Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled by setting value as
     * true and verify the assigned value using WebPA get command</li>
     * 
     * @param device
     *            Dut to be used
     * @Refactor Alan_Bivera
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1034", testDecription = "Verification of 2.4Ghz public wifi  been broadcasted by using this parameter 'Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled' using WebPA")
    public void testValidateSSIDAdvertisementEnabledFor2_4GhzPublicNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-134";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// Stores SNMP output
	String snmpOutput = null;
	// stores the public wifi//
	boolean publicWifiEnable = false;
	// stores the Access point status
	boolean isAccessPointEnabled = false;

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION :DESCRIPTION :Check and Enable 2.4GHz Public wifi using WebPA.");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION : ACTION : Check and Enable 2.4GHz Public wifi SSID");
	    LOGGER.info("PRE-CONDITION : EXPECTED :  2.4GHz public wifi SSID should be enabled successfully");

	    publicWifiEnable = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ENABLE_STATUS)
		    && BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ENABLE_STATUS);
	    LOGGER.info("Public wifi enabling is  " + publicWifiEnable);
	    if (!publicWifiEnable) {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "Public Wifi 2.4ghz is not enabled");
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-WEBPA-1034");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verification of 2.4Ghz Public wifi  been broadcasted by using this parameter 'Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled' using WebPA");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled by setting
	     * value as false and verify the assigned value using WebPA get command
	     */
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled by setting value as false and verify the assigned value using WebPA get command");
	    LOGGER.info(
		    "EXPECTED: TR181 parameter:Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled should be disabled with Success Message and Staus value should be 'false'");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_AccessPoint_10003_SSIDAdvertisementEnabled,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
	    errorMessage = "Unable to disable the TR-181 parameter 'Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled' by setting value as false";
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully disabled the TR-181 parameter 'Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled' by setting value as false"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:Enable the TR181 parameter Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled by setting value
	     * as true and verify the assigned value using WebPA get command
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Enable the TR181 parameter Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled by setting value as false and verify the assigned value using WebPA get command");
	    LOGGER.info(
		    "EXPECTED: TR181 parameter:Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled should be enabled with Success Message and Staus value should be 'true'");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_AccessPoint_10003_SSIDAdvertisementEnabled,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	    if (status) {
		isAccessPointEnabled = true;
	    }
	    errorMessage = "Unable to enable the TR-181 parameter 'Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled' by setting value as true";
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully enabled the TR-181 parameter 'Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled' by setting value as true"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while verifying the  2.4Ghz Public wifi 'Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	} finally {
	    if (!isAccessPointEnabled) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		LOGGER.info("#####################################################################################");
		LOGGER.info(
			"POST-CONDITION : DESCRIPTION : Reverting back to the original state  by setting the value as 'true' for the TR181 parameter 'Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled'");
		LOGGER.info(
			"POST-CONDITION : ACTION : TR181 parameter 'Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled' should be set as true");
		LOGGER.info(
			"POST-CONDITION :EXPECTED: Should be able to set as true to the TR181 parameter 'Device.WiFi.AccessPoint.10003.SSIDAdvertisementEnabled' successfully ");
		LOGGER.info("#####################################################################################");
		boolean result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_AccessPoint_10003_SSIDAdvertisementEnabled,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
		LOGGER.info("### POST-CONDITION  ### END POST-CONFIGURATIONS ON THE DEVICE: " + result);
	    }
	}
    }

    /**
     * This test case checks the SSIDAdvertisementEnabled on 5Ghz Public network by WebPA and verifies by SNMP
     * 
     * <ol>
     * <li>STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled by setting value as
     * false and verify the assigned value using WebPA get command</li>
     * <li>STEP 2:Enable the TR181 parameter Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled by setting value as
     * true and verify the assigned value using WebPA get command</li>
     * 
     * @param device
     *            Dut to be used
     * @Refactor Alan_Bivera
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1035", testDecription = "Verification of 5Ghz Public wifi  been broadcasted by using this parameter 'Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled' using WebPA")
    public void testValidateSSIDAdvertisementEnabledFor5GhzPublicNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-135";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// Stores SNMP output
	String snmpOutput = null;
	// stores the public wifi//
	boolean publicWifiEnable = false;
	// stores the Access point status
	boolean isAccessPointEnabled = false;
	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION :DESCRIPTION :Check and Enable 5GHz Public wifi using WebPA.");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION : ACTION : Check and Enable 5GHz Public wifi SSID");
	    LOGGER.info("PRE-CONDITION : EXPECTED :  5GHz public wifi SSID should be enabled successfully");

	    publicWifiEnable = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS)
		    && BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS);
	    LOGGER.info("Public wifi enabling is  " + publicWifiEnable);
	    if (!publicWifiEnable) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "Public Wifi 5ghz is not enabled");
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-WEBPA-1035");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verification of 5Ghz Public wifi  been broadcasted by using this parameter 'Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled' using WebPA");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled by setting
	     * value as false and verify the assigned value using WebPA get command
	     */
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Disable the TR181 parameter Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled by setting value as false and verify the assigned value using WebPA get command");
	    LOGGER.info(
		    "EXPECTED: TR181 parameter:Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled should be disabled with Success Message and Staus value should be 'false'");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_AccessPoint_10103_SSIDAdvertisementEnabled,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
	    errorMessage = "Unable to disable the TR-181 parameter 'Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled' by setting value as false";
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully disabled the TR-181 parameter 'Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled' by setting value as false"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:Enable the TR181 parameter �Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled� by setting
	     * value as true and verify the assigned value using WebPA get command
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Disable the TR181 parameter �Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled� by setting value as false and verify the assigned value using WebPA get command");
	    LOGGER.info(
		    "EXPECTED: TR181 parameter:Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled should be disabled with Success Message and Staus value should be 'false'");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_AccessPoint_10103_SSIDAdvertisementEnabled,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	    if (status) {
		isAccessPointEnabled = true;
	    }
	    errorMessage = "Unable to enable the TR-181 parameter 'Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled' by setting value as true";
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully enabled the TR-181 parameter 'Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled' by setting value as true"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while verifying the  5Ghz Public wifi 'Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled' using WebPA : "
		    + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	} finally {
	    if (!isAccessPointEnabled) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		LOGGER.info("#####################################################################################");
		LOGGER.info(
			"POST-CONDITION : DESCRIPTION : Reverting back to the original state  by setting the value as 'true' for the TR181 parameter 'Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled'");
		LOGGER.info(
			"POST-CONDITION : ACTION : TR181 parameter 'Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled' should be set as true");
		LOGGER.info(
			"POST-CONDITION :EXPECTED: Should be able to set as true to the TR181 parameter 'Device.WiFi.AccessPoint.10103.SSIDAdvertisementEnabled' successfully ");
		LOGGER.info("#####################################################################################");
		boolean result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_AccessPoint_10103_SSIDAdvertisementEnabled,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
		LOGGER.info("### POST-CONDITION  ### END POST-CONFIGURATIONS ON THE DEVICE: " + result);
	    }
	}
    }

    /**
     * To validate all the Security modes
     * 
     * <ol>
     * <li>STEP 1:Execute webPA command to Get on the Security Mode to get the current mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to set Security Mode parameter</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change.</li>
     * 
     * <li>STEP 4 :Execute WebPA command to set the current security mode parameter</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Athira
     */

    public void checkForSecurityModeParameter(Dut device, String testCaseId, String securityModeEnabled) {
	// String to store the test step number
	String testStepNumber = null;
	// String to store the test case ID
	String testId = testCaseId;
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// String to store the Current security mode
	String currentSecurityMode = "";
	// list of strings to store the supported security modes
	List<String> supportedSecurityModesList = new ArrayList<String>();
	supportedSecurityModesList.add(BroadBandTestConstants.SECURITY_MODE_NONE);
	supportedSecurityModesList.add(BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
	supportedSecurityModesList.add(BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE);

	// stores the step number
	int stepNumber = 0;
	try {

	    /**
	     * STEP 1:Execute webPA command to Get on the Security Mode to get the current mode
	     */
	    testStepNumber = "s1";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Execute  webPA Get command on the Security ModeEnabled parameter to get the current mode");
	    LOGGER.info("EXPECTED: Should retrieve the current security modes in the device");
	    LOGGER.info("#####################################################################################");
	    currentSecurityMode = tapEnv.executeWebPaCommand(device, securityModeEnabled);
	    LOGGER.info("CURRENT SECURITY MODE SET IN THE DEVICE " + "=" + currentSecurityMode);
	    status = CommonMethods.isNotNull(currentSecurityMode);
	    errorMessage = "webPA Get on Security Mode parmater gives empty or null response for WebPA command";
	    LOGGER.info(
		    "S1 ACTUAL : " + (status ? "webPA Get on Security Mode parameters to get the current mode - SUCCESS"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 2 : Execute WebPA command to set Security Mode parameter
	     */
	    for (int count = 0; count < supportedSecurityModesList.size(); count++) {

		if (count == 0) {
		    stepNumber = 2 + count;
		}
		if (currentSecurityMode.equalsIgnoreCase(supportedSecurityModesList.get(count))) {
		    continue;
		}
		testStepNumber = "s" + (stepNumber);
		status = false;
		LOGGER.info("#####################################################################################");
		LOGGER.info(testStepNumber + " " + ":Execute WebPA command to set Security Mode parameter-"
			+ securityModeEnabled + "to value as " + "-" + supportedSecurityModesList.get(count));
		LOGGER.info("EXPECTED: Device Should be able to set the security mode to"
			+ supportedSecurityModesList.get(count) + "for security mode parameter-" + securityModeEnabled);
		LOGGER.info("#####################################################################################");
		String timeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv, device);
		LOGGER.info("######timeStamp is###########" + timeStamp);
		status = BroadBandWebPaUtils.setAndVerifyAllSecurityModesSupported(device, tapEnv, securityModeEnabled,
			supportedSecurityModesList.get(count));
		stepNumber++;
		errorMessage = "WebPA Set on the security mode parameter got failed";
		;
		LOGGER.info(testStepNumber + " " + "ACTUAL : "
			+ (status
				? "webPA Set on Security ModeEnabled parameter as" + " "
					+ supportedSecurityModesList.get(count) + "-SUCCESS"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		/**
		 * STEP 3 : Verify whether device logs the telemetry marker for security mode change.
		 * 
		 */
		testStepNumber = "s" + stepNumber;
		status = false;
		LOGGER.info("#####################################################################################");
		LOGGER.info(testStepNumber + " "
			+ "Verify whether device logs the telemetry marker for security mode change to-"
			+ supportedSecurityModesList.get(count));
		LOGGER.info("Device should logs the telemetry marker for security mode change.");
		LOGGER.info("#####################################################################################");
		status = BroadBandWebPaUtils.checkingForTelemetryMarkerInDeviceAfterSettingSecurityModes(device, tapEnv,
			supportedSecurityModesList.get(count), timeStamp);
		stepNumber++;
		errorMessage = "Appropriate Telemetry marker is not logged when Secuity mode is set to "
			+ supportedSecurityModesList.get(count);

		LOGGER.info(testStepNumber + " " + " ACTUAL : "
			+ (status
				? "Appropriate Telemetry marker is logged when Secuity mode is set to " + " "
					+ supportedSecurityModesList.get(count) + " - SUCCESS"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	    }

	    /**
	     * STEP 4 :Execute WebPA command to set the current security mode parameter
	     * 
	     */
	    testStepNumber = "s" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(testStepNumber + " " + ": Execute WebPA command to set security mode parameter - "
		    + securityModeEnabled + " with value of current security mode as" + "-" + currentSecurityMode);
	    LOGGER.info("EXPECTED: Should be able to set the current security mode to " + currentSecurityMode
		    + " for security mode parameter - " + securityModeEnabled);
	    LOGGER.info("#####################################################################################");
	    String timeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv, device);
	    LOGGER.info("######timeStamp is###########" + timeStamp);
	    stepNumber++;
	    status = BroadBandWebPaUtils.setAndVerifyAllSecurityModesSupported(device, tapEnv, securityModeEnabled,
		    currentSecurityMode);

	    errorMessage = "WebPA Set on the current modeEnabled parameter got failed";
	    LOGGER.info(testStepNumber + " " + "ACTUAL : "
		    + (status
			    ? "webPA Set on current Security ModeEnabled parameter as " + currentSecurityMode
				    + "-SUCCESS"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 5: Verify whether device logs the telemetry marker for current security mode change
	     * 
	     */
	    testStepNumber = "s" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(testStepNumber + " "
		    + "Verify whether device logs the telemetry marker for current security mode change to-"
		    + currentSecurityMode);
	    LOGGER.info("EXPECTED: Device should log the telemetry marker for current security mode change");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.checkingForTelemetryMarkerInDeviceAfterSettingSecurityModes(device, tapEnv,
		    currentSecurityMode, timeStamp);

	    errorMessage = "Appropriate Telemetry marker is not logged when Secuity mode is set to "
		    + currentSecurityMode;

	    LOGGER.info(testStepNumber + " " + " ACTUAL : "
		    + (status
			    ? "Appropriate Telemetry marker is logged when Secuity mode is set to" + " "
				    + currentSecurityMode + " - SUCCESS"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception exception) {

	    LOGGER.error("Exception occured in verifying Security.ModeEnabled parameters" + exception.getMessage());
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	}

    }
}
